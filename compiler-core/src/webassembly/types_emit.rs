//! User-type emission via Tarjan SCC + walrus `add_rec_group`.
//!
//! Each Gleam type is mapped to one or more WASM type nodes:
//!
//! - Plain struct / tuple / function: 1 node each.
//! - Union: 1 supertype node + N subtype nodes (one per variant).
//!
//! `ensure_emitted_lazy(root)` (defined on `Generator`) walks the type graph
//! starting at `root`, collecting `TypeNode` descriptors via `visit_type_node`
//! (metadata only — never the expression AST). The resulting batch is handed
//! to `emit_type_nodes`, which builds the dep graph between nodes, runs
//! [`tarjan_scc`] to obtain SCCs in reverse topological order, and emits each
//! SCC either as a singleton (via `add_struct`/`add_composite`/`add`,
//! preserving arena-level dedup) or via `add_rec_group` when the SCC has a
//! cycle or self-loop.
//!
//! This module owns the descriptor types and pure helpers (Tarjan, composite
//! builder, val-type resolvers). The orchestration methods live in
//! `webassembly.rs` because they need access to private `Generator` state.
//!
//! Lazy entry: `val_type`/`type_index` consult `lookup_node_*`; on cache miss
//! they call `ensure_emitted_lazy`, then re-query. Variants whose fields
//! still contain type variables for this instantiation are skipped — they are
//! emitted later when a concrete instantiation appears.

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use ecow::EcoString;
use walrus::{FieldType, HeapType, RefType, StorageType, TypeId, ValType};

use crate::type_::Type;

use super::instructions::{FloatType, IntType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum TypeNodeKey {
    Plain {
        name: EcoString,
    },
    UnionSuper {
        name: EcoString,
        shared_field_types: Vec<EcoString>,
    },
    UnionSub {
        pretty: EcoString,
        constructor: EcoString,
    },
    Function {
        params: Vec<EcoString>,
        results: Vec<EcoString>,
    },
}

/// Discriminates the WASM-level shape of a `TypeNode` and carries the data
/// each variant actually needs. Named composites (struct / tuple / union)
/// know their walrus name; `Function` carries the number of result types
/// (the tail slice of `field_types`).
#[derive(Debug, Clone)]
pub(super) enum TypeNodeKind {
    PlainStruct { name: EcoString },
    Tuple { name: EcoString },
    UnionSupertype { name: EcoString },
    UnionSubtype { name: EcoString },
    Function { result_count: usize },
}

impl TypeNodeKind {
    /// Walrus name section entry for this kind, if any. Function types are
    /// not named in the type section.
    pub(super) fn walrus_name(&self) -> Option<&EcoString> {
        match self {
            TypeNodeKind::PlainStruct { name }
            | TypeNodeKind::Tuple { name }
            | TypeNodeKind::UnionSupertype { name }
            | TypeNodeKind::UnionSubtype { name } => Some(name),
            TypeNodeKind::Function { .. } => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct TypeNode {
    pub(super) key: TypeNodeKey,
    pub(super) kind: TypeNodeKind,
    pub(super) is_final: bool,
    pub(super) supertype_key: Option<TypeNodeKey>,
    pub(super) field_types: Vec<Arc<Type>>,
    pub(super) field_labels: Vec<EcoString>,
}

/// Shared state for the `visit_type_node`/`visit_union_type_nodes` graph
/// walk in `webassembly.rs`. Grouped so the recursive helpers take one
/// `&mut VisitState` instead of three parallel mutable arguments.
pub(super) struct VisitState {
    pub(super) visited: HashSet<TypeNodeKey>,
    pub(super) descriptors: Vec<TypeNode>,
    pub(super) to_visit: Vec<Arc<Type>>,
}

impl VisitState {
    pub(super) fn new(visited: HashSet<TypeNodeKey>, root: Arc<Type>) -> Self {
        Self {
            visited,
            descriptors: Vec::new(),
            to_visit: vec![root],
        }
    }
}

/// How a field of a `TypeNode` resolves to a WASM `ValType` during emission.
/// The three variants are mutually exclusive — using an enum keeps that
/// invariant in the type system instead of as a comment.
#[derive(Debug, Clone, Copy)]
pub(super) enum FieldRef {
    /// Primitive, external, or enum field — already resolved to a ValType.
    Direct(ValType),
    /// Field type is a node in the same batch (will be assigned a TypeId
    /// during SCC emission).
    InBatch { idx: usize, nullable: bool },
    /// Field type was emitted in a prior batch and lives in the cache.
    Cached { id: TypeId, nullable: bool },
}

/// Tarjan's SCC algorithm — iterative DFS so the Rust call stack does not
/// limit how deep the type graph can go. Returns SCCs in reverse topological
/// order (post-order: leaves first, roots last).
///
/// All node indices come from `0..node_count` (either start values from
/// the outer loop, or neighbors taken from `edges`, which is assumed
/// well-formed by the caller). Direct indexing of `State`'s vectors is
/// therefore safe and that's how the body is written.
#[allow(clippy::indexing_slicing)]
pub(super) fn tarjan_scc(node_count: usize, edges: &[Vec<usize>]) -> Vec<Vec<usize>> {
    struct State {
        index_of: Vec<Option<usize>>,
        lowlink: Vec<usize>,
        on_stack: Vec<bool>,
        stack: Vec<usize>,
        next_index: usize,
        sccs: Vec<Vec<usize>>,
    }

    impl State {
        /// First visit of node `v`: assign index/lowlink, push to SCC stack.
        fn enter(&mut self, v: usize) {
            self.index_of[v] = Some(self.next_index);
            self.lowlink[v] = self.next_index;
            self.next_index += 1;
            self.on_stack[v] = true;
            self.stack.push(v);
        }

        /// Pull `candidate` into v's lowlink: the Tarjan "merge" step.
        fn propagate_lowlink(&mut self, v: usize, candidate: usize) {
            if let Some(slot) = self.lowlink.get_mut(v) {
                *slot = (*slot).min(candidate);
            }
        }

        /// Pop the SCC rooted at `root` off the stack and record it.
        fn pop_scc(&mut self, root: usize) {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().expect("tarjan: stack non-empty");
                self.on_stack[w] = false;
                scc.push(w);
                if w == root {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }

    /// One frame of the simulated call stack: the node and the index of
    /// the next neighbor to consider in `edges[v]`.
    struct Frame {
        v: usize,
        next_neighbor: usize,
    }

    let mut state = State {
        index_of: vec![None; node_count],
        lowlink: vec![0; node_count],
        on_stack: vec![false; node_count],
        stack: Vec::new(),
        next_index: 0,
        sccs: Vec::new(),
    };
    let mut frames: Vec<Frame> = Vec::new();
    let neighbors = |v: usize| edges.get(v).map(Vec::as_slice).unwrap_or(&[]);

    for start in 0..node_count {
        if state.index_of[start].is_some() {
            continue;
        }
        state.enter(start);
        frames.push(Frame {
            v: start,
            next_neighbor: 0,
        });

        while !frames.is_empty() {
            let top = frames.len() - 1;
            let v = frames[top].v;
            let next_neighbor = frames[top].next_neighbor;

            if let Some(&w) = neighbors(v).get(next_neighbor) {
                frames[top].next_neighbor += 1;
                match state.index_of[w] {
                    None => {
                        state.enter(w);
                        frames.push(Frame {
                            v: w,
                            next_neighbor: 0,
                        });
                    }
                    // Back-edge to an active node: pull w's index into v.lowlink.
                    Some(w_idx) if state.on_stack[w] => {
                        state.propagate_lowlink(v, w_idx);
                    }
                    // Cross-edge to a finished SCC: nothing to propagate.
                    Some(_) => {}
                }
                continue;
            }

            // All neighbors processed: finalize v, then propagate v's
            // lowlink to the parent (the equivalent of the recursive return).
            let v_low = state.lowlink[v];
            let v_idx = state.index_of[v].expect("v has index");
            if v_low == v_idx {
                state.pop_scc(v);
            }
            let _ = frames.pop();
            if let Some(parent) = frames.last() {
                state.propagate_lowlink(parent.v, v_low);
            }
        }
    }

    state.sccs
}

#[cfg(test)]
#[test]
fn test_tarjan_scc() {
    // Simple cycle: 0 → 1 → 0
    let sccs = tarjan_scc(2, &[vec![1], vec![0]]);
    assert_eq!(sccs.len(), 1);
    assert_eq!(sccs[0].len(), 2);

    // DAG: 0 → 1 → 2 (3 singleton SCCs in reverse topo order: 2, 1, 0)
    let sccs = tarjan_scc(3, &[vec![1], vec![2], vec![]]);
    assert_eq!(sccs, vec![vec![2], vec![1], vec![0]]);

    // Self-loop: 0 → 0
    let sccs = tarjan_scc(1, &[vec![0]]);
    assert_eq!(sccs, vec![vec![0]]);

    // Mixed: 0 → 1, 1 → 2, 2 → 1 (SCCs: {1,2}, {0})
    let sccs = tarjan_scc(3, &[vec![1], vec![2], vec![1]]);
    assert_eq!(sccs.len(), 2);
    let mut first: Vec<usize> = sccs[0].clone();
    first.sort();
    assert_eq!(first, vec![1, 2]);
    assert_eq!(sccs[1], vec![0]);
}

#[cfg(test)]
#[test]
fn test_tarjan_scc_deep_chain() {
    // A linear chain 0 → 1 → 2 → ... → N-1. Depth N. With the recursive
    // version this would risk a Rust stack overflow at high N; the iterative
    // version handles it with a heap-allocated frame stack.
    const N: usize = 50_000;
    let edges: Vec<Vec<usize>> = (0..N)
        .map(|i| if i + 1 < N { vec![i + 1] } else { vec![] })
        .collect();
    let sccs = tarjan_scc(N, &edges);
    assert_eq!(sccs.len(), N);
    // Post-order: leaf (N-1) comes out first.
    assert_eq!(sccs.first(), Some(&vec![N - 1]));
    assert_eq!(sccs.last(), Some(&vec![0]));
}

#[cfg(test)]
#[test]
fn test_tarjan_scc_deep_cycle() {
    // A single big cycle 0 → 1 → ... → N-1 → 0. One SCC of size N.
    const N: usize = 50_000;
    let edges: Vec<Vec<usize>> = (0..N).map(|i| vec![(i + 1) % N]).collect();
    let sccs = tarjan_scc(N, &edges);
    assert_eq!(sccs.len(), 1);
    assert_eq!(sccs[0].len(), N);
}

/// Resolve a `FieldRef` to its `ValType`.
///
/// Inside an `add_rec_group` closure, `ids` holds the freshly-allocated
/// TypeIds for the SCC and `local_of` maps an `InBatch.idx` to its position
/// in `ids`. Outside the closure (acyclic singleton emission, or post-publish
/// in `emit_cyclic_scc`), all SCC IDs are in `assigned` and `ids`/`local_of`
/// can be empty.
pub(super) fn resolve_val_type(
    info: FieldRef,
    ids: &[TypeId],
    assigned: &[Option<TypeId>],
    local_of: &HashMap<usize, usize>,
) -> ValType {
    let (id, nullable) = match info {
        FieldRef::Direct(vt) => return vt,
        FieldRef::Cached { id, nullable } => (id, nullable),
        FieldRef::InBatch { idx, nullable } => {
            let id = match local_of.get(&idx) {
                Some(&local) => *ids.get(local).expect("emit: rec_group id"),
                None => assigned
                    .get(idx)
                    .copied()
                    .flatten()
                    .expect("emit: in-batch ref not yet emitted"),
            };
            (id, nullable)
        }
    };
    ValType::Ref(RefType {
        heap_type: HeapType::Concrete(id),
        nullable,
    })
}

pub(super) fn primitive_val_type(
    t: &Arc<Type>,
    int: IntType,
    float: FloatType,
    string_idx: TypeId,
) -> Option<ValType> {
    if t.is_int() {
        return Some(int.val_type());
    }
    if t.is_float() {
        return Some(float.val_type());
    }
    if t.is_string() {
        return Some(ValType::Ref(RefType {
            heap_type: HeapType::Concrete(string_idx),
            nullable: false,
        }));
    }
    if t.is_bool() || t.is_utf_codepoint() {
        return Some(ValType::I32);
    }
    if let Some((_, n)) = t.named_type_name()
        && n.as_str() == "Nil"
    {
        return Some(ValType::I32);
    }
    None
}

/// Magic-name externals (`pub type I32 {}` and friends) map directly to
/// WASM numeric types. Called before the `Enum` fall-through, which would
/// otherwise force them all to `I32`.
pub(super) fn named_numeric_val_type(t: &Arc<Type>) -> Option<ValType> {
    let (_, name) = t.named_type_name()?;
    Some(match name.as_str() {
        "I32" => ValType::I32,
        "I64" => ValType::I64,
        "F32" => ValType::F32,
        "F64" => ValType::F64,
        _ => return None,
    })
}

pub(super) fn build_key_index(descriptors: &[TypeNode]) -> HashMap<TypeNodeKey, usize> {
    descriptors
        .iter()
        .enumerate()
        .map(|(i, d)| (d.key.clone(), i))
        .collect()
}

/// Edges between batch nodes: `InBatch` field refs and subtype → supertype.
/// `Direct` and `Cached` field refs don't produce edges (they're already
/// resolved).
pub(super) fn build_dep_edges(
    descriptors: &[TypeNode],
    field_refs: &[Vec<FieldRef>],
    key_to_idx: &HashMap<TypeNodeKey, usize>,
) -> Vec<Vec<usize>> {
    descriptors
        .iter()
        .zip(field_refs.iter())
        .map(|(d, refs)| {
            let mut deps: Vec<usize> = refs
                .iter()
                .filter_map(|r| match r {
                    FieldRef::InBatch { idx, .. } => Some(*idx),
                    FieldRef::Direct(_) | FieldRef::Cached { .. } => None,
                })
                .collect();
            if let Some(super_key) = &d.supertype_key
                && let Some(&super_idx) = key_to_idx.get(super_key)
            {
                deps.push(super_idx);
            }
            deps
        })
        .collect()
}

/// Where to find a node's supertype `TypeId` during SCC emission.
/// Pre-resolved by the caller before entering `add_rec_group`, because the
/// walrus closure holds an exclusive borrow of the type arena and can't
/// reach back into `Generator::type_node_cache`.
#[derive(Debug, Clone, Copy)]
pub(super) enum SupertypeSrc {
    /// Same SCC: resolved via `ids[local]` inside the closure.
    InScc(usize),
    /// Prior SCC in this batch, or a previous batch (cache).
    External(TypeId),
}

/// Resolve a pre-computed supertype reference into a concrete `TypeId`.
/// `ids` is the slice walrus passes into the `add_rec_group` closure.
pub(super) fn resolve_supertype_id(src: Option<SupertypeSrc>, ids: &[TypeId]) -> Option<TypeId> {
    src.map(|s| match s {
        SupertypeSrc::InScc(local) => *ids.get(local).expect("scc-local supertype id"),
        SupertypeSrc::External(id) => id,
    })
}

/// Split a Function-kind node's `val_types` into (params, results).
/// `saturating_sub` keeps the split well-defined even if the slice is
/// somehow shorter than `result_count`; in practice they match exactly.
pub(super) fn split_function_val_types(
    val_types: &[ValType],
    result_count: usize,
) -> (&[ValType], &[ValType]) {
    let param_count = val_types.len().saturating_sub(result_count);
    val_types.split_at(param_count)
}

pub(super) fn build_composite_for_node(
    kind: &TypeNodeKind,
    val_types: &[ValType],
) -> walrus::CompositeType {
    fn immutable(vt: ValType) -> FieldType {
        FieldType {
            element_type: StorageType::Val(vt),
            mutable: false,
        }
    }
    fn struct_(fields: Vec<FieldType>) -> walrus::CompositeType {
        walrus::CompositeType::Struct(walrus::StructType {
            fields: fields.into_boxed_slice(),
        })
    }
    match kind {
        TypeNodeKind::PlainStruct { .. } | TypeNodeKind::Tuple { .. } => {
            struct_(val_types.iter().copied().map(immutable).collect())
        }
        TypeNodeKind::UnionSupertype { .. } | TypeNodeKind::UnionSubtype { .. } => {
            // Union structs lead with an I32 tag, then the variant's fields.
            let tag = std::iter::once(immutable(ValType::I32));
            struct_(
                tag.chain(val_types.iter().copied().map(immutable))
                    .collect(),
            )
        }
        TypeNodeKind::Function { result_count } => {
            let (params, results) = split_function_val_types(val_types, *result_count);
            walrus::CompositeType::Function(walrus::FunctionType::new(
                params.to_vec().into_boxed_slice(),
                results.to_vec().into_boxed_slice(),
            ))
        }
    }
}
