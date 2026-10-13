// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Clears the reference locals after their last use.
//!
//! An engine without liveness analysis (like the baseline compiler of V8)
//! keeps alive everything a local of a running function references, even when
//! the local is not used anymore. A big value that dies young then survives
//! the collections that happen while the function runs.
//!
//! The analysis walks each sequence of instructions backwards, keeping the
//! locals that are read later (live). A `local.get` of a local that is not live
//! is its last use, and is followed by a `local.set` of null. A local that is
//! read only when a branch is taken is also cleared after the branch. A local
//! is cleared only when a call that can allocate happens after its last use,
//! as the frame is short lived otherwise, and a collection starts only on an
//! allocation. A local whose value is referenced by a param (like the captures
//! of a closure) is not cleared, as that frees nothing.

// The matches on instructions only care about a few of them.
#![allow(clippy::wildcard_enum_match_arm)]

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use walrus::ir::{
    BrIf, BrOnCast, BrOnCastFail, BrOnNonNull, BrOnNull, Instr, InstrLocId, InstrSeqId, Local,
    LocalSet, LocalTee, RefAsNonNull, RefNull,
};
use walrus::{
    AbstractHeapType, CompositeType, FunctionId, HeapType, LocalFunction, LocalId, ModuleLocals,
    ModuleTypes, RefType, StorageType, ValType,
};

pub(super) fn clear_dead_locals(module: &mut walrus::Module) {
    let mut widened = HashSet::new();
    // A call to a function that calls nothing and has no loops is short, so
    // it is not a reason to clear a local.
    let mut leaves: HashSet<FunctionId> = module
        .funcs
        .iter_local()
        .filter(|(_, f)| is_leaf(f, f.entry_block()))
        .map(|(id, _)| id)
        .collect();
    // A call to a function that does not allocate cannot start a collection,
    // so it is not a reason to clear a local either.
    leaves.extend(not_allocating(module));
    let locals = &module.locals;
    let types = &module.types;
    for (_, function) in module.funcs.iter_local_mut() {
        // Gleam functions have no loops, so a single backward walk is enough.
        if has_loop(function, function.entry_block()) {
            continue;
        }
        let mut analysis = Analysis {
            locals,
            types,
            args: function.args.iter().copied().collect(),
            pinned: pinned(function, locals, types),
            leaves: &leaves,
            labels: HashMap::new(),
            widened: HashSet::new(),
        };
        let entry = function.entry_block();
        let _ = analysis.seq(function, entry, State::default());
        // A non-nullable local that is cleared becomes nullable, and its reads
        // are asserted to be non-null.
        if !analysis.widened.is_empty() {
            assert_non_null(function, entry, &analysis.widened);
            widened.extend(analysis.widened);
        }
    }
    for id in widened {
        let local = module.locals.get_mut(id);
        let ValType::Ref(ty) = local.ty() else {
            unreachable!()
        };
        let name = local.name.take();
        *local = Local::new(
            id,
            ValType::Ref(RefType {
                nullable: true,
                ..ty
            }),
        );
        local.name = name;
    }
}

fn assert_non_null(function: &mut LocalFunction, seq: InstrSeqId, widened: &HashSet<LocalId>) {
    let instrs = std::mem::take(&mut function.block_mut(seq).instrs);
    let mut result = Vec::with_capacity(instrs.len());
    for (i, (instr, loc)) in instrs.iter().enumerate() {
        match instr {
            Instr::Block(b) => assert_non_null(function, b.seq, widened),
            Instr::IfElse(i) => {
                assert_non_null(function, i.consequent, widened);
                assert_non_null(function, i.alternative, widened);
            }
            _ => {}
        }
        let get = match instr {
            Instr::LocalGet(get) => widened.contains(&get.local),
            Instr::LocalTee(tee) => widened.contains(&tee.local),
            _ => false,
        };
        result.push((instr.clone(), *loc));
        if get && !accepts_null(instrs.get(i + 1..).unwrap_or_default()) {
            result.push((Instr::RefAsNonNull(RefAsNonNull {}), InstrLocId::default()));
        }
    }
    function.block_mut(seq).instrs = result;
}

/// Whether the next value consumed by `instrs` can be null. The value of a
/// cleared local is never null where it is read, so it needs to be asserted
/// only where its type matters.
fn accepts_null(instrs: &[(Instr, InstrLocId)]) -> bool {
    match instrs {
        // Skips the clearing of the local after its last use.
        [(Instr::RefNull(_), _), (Instr::LocalSet(_), _), rest @ ..] => accepts_null(rest),
        [(instr, _), ..] => matches!(
            instr,
            Instr::StructGet(_)
                | Instr::StructGetS(_)
                | Instr::StructGetU(_)
                | Instr::ArrayGet(_)
                | Instr::ArrayGetS(_)
                | Instr::ArrayGetU(_)
                | Instr::ArrayLen(_)
                | Instr::RefCast(_)
                | Instr::RefTest(_)
        ),
        [] => false,
    }
}

/// Whether a value of the heap type is always small. Clearing a local with
/// such a value frees only the value itself, as it references nothing.
fn is_small(types: &ModuleTypes, heap_type: HeapType) -> bool {
    let (id, subtypes) = match heap_type {
        HeapType::Abstract(
            AbstractHeapType::Func
            | AbstractHeapType::NoFunc
            | AbstractHeapType::I31
            | AbstractHeapType::None
            | AbstractHeapType::NoExtern
            | AbstractHeapType::NoExn,
        ) => return true,
        HeapType::Concrete(id) => (id, !types.get(id).is_final),
        HeapType::Exact(id) => (id, false),
        _ => return false,
    };
    match types.get(id).kind() {
        CompositeType::Function(_) => true,
        CompositeType::Array(_) => false,
        // A subtype can have more fields.
        CompositeType::Struct(struct_) => {
            !subtypes
                && struct_
                    .fields
                    .iter()
                    .all(|field| !matches!(field.element_type, StorageType::Val(ValType::Ref(_))))
        }
    }
}

/// The params that are not cleared and the locals set at the start of the
/// function with values they reference (like the environment and the
/// captures of a closure). Clearing them frees nothing.
fn pinned(
    function: &LocalFunction,
    locals: &ModuleLocals,
    types: &ModuleTypes,
) -> HashSet<LocalId> {
    let mut sets = HashMap::new();
    count_sets(function, function.entry_block(), &mut sets);
    let mut pinned: HashSet<LocalId> = function
        .args
        .iter()
        .copied()
        .filter(|arg| {
            !sets.contains_key(arg)
                && matches!(locals.get(*arg).ty(), ValType::Ref(ty) if !ty.nullable)
        })
        .collect();
    // Whether each value on the stack is referenced by a pinned local.
    let mut stack = vec![];
    for (instr, _) in &function.block(function.entry_block()).instrs {
        match instr {
            Instr::LocalGet(get) => stack.push(pinned.contains(&get.local)),
            Instr::RefCast(_) => {}
            Instr::StructGet(get) => {
                let mutable = match types.get(get.ty).kind() {
                    CompositeType::Struct(struct_) => struct_
                        .fields
                        .get(get.field as usize)
                        .is_none_or(|field| field.mutable),
                    _ => true,
                };
                let value = stack.pop().unwrap_or_default();
                stack.push(value && !mutable);
            }
            Instr::LocalSet(set) => {
                if stack.pop().unwrap_or_default() && sets.get(&set.local) == Some(&1) {
                    let _ = pinned.insert(set.local);
                }
            }
            _ => break,
        }
    }
    pinned
}

/// Counts the `local.set` and `local.tee` of each local.
fn count_sets(function: &LocalFunction, seq: InstrSeqId, sets: &mut HashMap<LocalId, usize>) {
    for (instr, _) in &function.block(seq).instrs {
        match instr {
            Instr::LocalSet(LocalSet { local }) | Instr::LocalTee(LocalTee { local }) => {
                *sets.entry(*local).or_default() += 1
            }
            Instr::Block(b) => count_sets(function, b.seq, sets),
            Instr::IfElse(i) => {
                count_sets(function, i.consequent, sets);
                count_sets(function, i.alternative, sets);
            }
            _ => {}
        }
    }
}

/// The functions that do not allocate, and call only functions that do not
/// allocate.
fn not_allocating(module: &walrus::Module) -> HashSet<FunctionId> {
    let mut callees = HashMap::new();
    for (id, function) in module.funcs.iter_local() {
        let mut calls = vec![];
        if !allocates(function, function.entry_block(), &mut calls) {
            let _ = callees.insert(id, calls);
        }
    }
    // Removes the functions that call one that allocates until none is left.
    loop {
        let allocating = callees
            .iter()
            .filter(|(_, calls)| calls.iter().any(|call| !callees.contains_key(call)))
            .map(|(id, _)| *id)
            .collect_vec();
        if allocating.is_empty() {
            return callees.into_keys().collect();
        }
        for id in allocating {
            let _ = callees.remove(&id);
        }
    }
}

/// Whether the sequence allocates or calls an unknown function. The known
/// functions it calls are pushed to `calls`.
fn allocates(function: &LocalFunction, seq: InstrSeqId, calls: &mut Vec<FunctionId>) -> bool {
    function
        .block(seq)
        .instrs
        .iter()
        .any(|(instr, _)| match instr {
            Instr::Call(call) => {
                calls.push(call.func);
                false
            }
            Instr::ReturnCall(call) => {
                calls.push(call.func);
                false
            }
            Instr::CallRef(_)
            | Instr::CallIndirect(_)
            | Instr::ReturnCallRef(_)
            | Instr::ReturnCallIndirect(_)
            | Instr::StructNew(_)
            | Instr::StructNewDefault(_)
            | Instr::ArrayNew(_)
            | Instr::ArrayNewDefault(_)
            | Instr::ArrayNewFixed(_)
            | Instr::ArrayNewData(_)
            | Instr::ArrayNewElem(_)
            | Instr::AnyConvertExtern(_)
            | Instr::ExternConvertAny(_)
            | Instr::MemoryGrow(_)
            | Instr::TableGrow(_)
            | Instr::Try(_)
            | Instr::TryTable(_) => true,
            Instr::Loop(l) => allocates(function, l.seq, calls),
            Instr::Block(b) => allocates(function, b.seq, calls),
            Instr::IfElse(i) => {
                allocates(function, i.consequent, calls)
                    || allocates(function, i.alternative, calls)
            }
            _ => false,
        })
}

fn is_leaf(function: &LocalFunction, seq: InstrSeqId) -> bool {
    function
        .block(seq)
        .instrs
        .iter()
        .all(|(instr, _)| match instr {
            Instr::Loop(_)
            | Instr::Try(_)
            | Instr::TryTable(_)
            | Instr::Call(_)
            | Instr::CallRef(_)
            | Instr::CallIndirect(_)
            | Instr::ReturnCall(_)
            | Instr::ReturnCallRef(_)
            | Instr::ReturnCallIndirect(_) => false,
            Instr::Block(b) => is_leaf(function, b.seq),
            Instr::IfElse(i) => is_leaf(function, i.consequent) && is_leaf(function, i.alternative),
            _ => true,
        })
}

fn has_loop(function: &LocalFunction, seq: InstrSeqId) -> bool {
    function
        .block(seq)
        .instrs
        .iter()
        .any(|(instr, _)| match instr {
            Instr::Loop(_) | Instr::Try(_) | Instr::TryTable(_) => true,
            Instr::Block(b) => has_loop(function, b.seq),
            Instr::IfElse(i) => {
                has_loop(function, i.consequent) || has_loop(function, i.alternative)
            }
            _ => false,
        })
}

/// What is known about the execution after a point.
#[derive(Clone, Default)]
struct State {
    /// The locals that are read.
    live: HashSet<LocalId>,
    /// Whether a call can happen before the function returns.
    call: bool,
}

impl State {
    fn union(mut self, other: &State) -> State {
        self.live.extend(other.live.iter().copied());
        self.call |= other.call;
        self
    }
}

struct Analysis<'a> {
    locals: &'a ModuleLocals,
    types: &'a ModuleTypes,
    args: HashSet<LocalId>,
    /// The locals that are not cleared.
    pinned: HashSet<LocalId>,
    leaves: &'a HashSet<FunctionId>,
    /// The state after a branch to each label.
    labels: HashMap<InstrSeqId, State>,
    /// The non-nullable locals that are cleared.
    widened: HashSet<LocalId>,
}

impl Analysis<'_> {
    /// The nullable reference type of a local that can be cleared.
    fn clearable(&mut self, local: LocalId) -> Option<RefType> {
        match self.locals.get(local).ty() {
            // The type of a parameter is in the signature, so only a nullable
            // one can be cleared.
            ValType::Ref(ty)
                if (ty.nullable || !self.args.contains(&local))
                    && !self.pinned.contains(&local)
                    && !is_small(self.types, ty.heap_type) =>
            {
                if !ty.nullable {
                    let _ = self.widened.insert(local);
                }
                Some(RefType {
                    nullable: true,
                    ..ty
                })
            }
            _ => None,
        }
    }

    /// The instructions that clear the `dead` locals, when a call can happen
    /// in `state`.
    fn clear(&mut self, state: &State, mut dead: Vec<LocalId>) -> Vec<(Instr, InstrLocId)> {
        if !state.call {
            return vec![];
        }
        // Sorted to generate the same code every time.
        dead.sort();
        let mut instrs = vec![];
        for local in dead {
            if let Some(ty) = self.clearable(local) {
                instrs.push((Instr::RefNull(RefNull { ty }), InstrLocId::default()));
                instrs.push((Instr::LocalSet(LocalSet { local }), InstrLocId::default()));
            }
        }
        instrs
    }

    fn label(&self, id: InstrSeqId) -> &State {
        self.labels
            .get(&id)
            .expect("branch to an enclosing sequence")
    }

    /// Walks the sequence `id` backwards from the `after` state and returns
    /// the state at its start.
    fn seq(&mut self, function: &mut LocalFunction, id: InstrSeqId, after: State) -> State {
        let _ = self.labels.insert(id, after.clone());
        let instrs = std::mem::take(&mut function.block_mut(id).instrs);
        let mut result = Vec::with_capacity(instrs.len());
        let mut state = after;
        for (instr, loc) in instrs.into_iter().rev() {
            match &instr {
                Instr::LocalGet(get) => {
                    if !state.live.contains(&get.local) {
                        result.extend(self.clear(&state, vec![get.local]).into_iter().rev());
                    }
                    let _ = state.live.insert(get.local);
                }
                Instr::LocalSet(LocalSet { local }) | Instr::LocalTee(LocalTee { local }) => {
                    let _ = state.live.remove(local);
                }
                Instr::Call(call) => state.call |= !self.leaves.contains(&call.func),
                Instr::CallRef(_) | Instr::CallIndirect(_) => state.call = true,
                Instr::Return(_)
                | Instr::ReturnCall(_)
                | Instr::ReturnCallRef(_)
                | Instr::ReturnCallIndirect(_)
                | Instr::Unreachable(_)
                | Instr::Throw(_)
                | Instr::ThrowRef(_) => state = State::default(),
                Instr::Br(br) => state = self.label(br.block).clone(),
                Instr::BrIf(BrIf { block })
                | Instr::BrOnNull(BrOnNull { block })
                | Instr::BrOnNonNull(BrOnNonNull { block })
                | Instr::BrOnCast(BrOnCast { block, .. })
                | Instr::BrOnCastFail(BrOnCastFail { block, .. }) => {
                    // The locals read only after the branch is taken are dead
                    // when it is not.
                    let dead = self.label(*block).live.difference(&state.live);
                    let dead = dead.copied().collect();
                    result.extend(self.clear(&state, dead).into_iter().rev());
                    state = state.union(self.label(*block))
                }
                Instr::BrTable(br) => {
                    state = br
                        .blocks
                        .iter()
                        .fold(self.label(br.default).clone(), |s, b| {
                            s.union(self.label(*b))
                        })
                }
                Instr::Block(block) => state = self.seq(function, block.seq, state),
                Instr::IfElse(if_else) => {
                    let consequent = self.seq(function, if_else.consequent, state.clone());
                    let alternative = self.seq(function, if_else.alternative, state);
                    state = consequent.union(&alternative);
                }
                _ => {}
            }
            result.push((instr, loc));
        }
        result.reverse();
        function.block_mut(id).instrs = result;
        state
    }
}
