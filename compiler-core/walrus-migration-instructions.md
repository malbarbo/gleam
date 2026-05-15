# Instruções para migração wasm-encoder → walrus

## Branches

- **`wasm-walrus-v2`** — branch de trabalho para a nova tentativa (parte do
  baseline limpo)
- **`wasm`** — tentativa anterior (commit `7fb2dccb9`). Usar como **referência
  para a lógica walrus**, mas NÃO copiar a estrutura/estilo do código.

## Objetivo

Migrar o backend WebAssembly do compilador Gleam de `wasm-encoder` para `walrus`,
minimizando o diff. O baseline é o commit `60a17fe97`. O diff final deve conter
apenas as mudanças estritamente necessárias para a troca de API.

## Princípio fundamental

**Cada linha que pode ficar idêntica ao original DEVE ficar idêntica.** Só mude o
que é estritamente necessário para a API walrus. Antes de mudar qualquer linha,
pergunte: "essa mudança é necessária para walrus funcionar?"

## Estratégia em dois passos

### Passo 1: Newtypes para u32

Antes de tocar no walrus, trocar todos os `u32` que representam índices por
newtypes:

```rust
struct FunctionIndex(u32);
struct TypeIndex(u32);
struct GlobalIndex(u32);
struct LocalIndex(u32);
```

Isso é um diff mecânico e pequeno. Depois, no passo 2, trocar
`FunctionIndex(u32)` por `walrus::FunctionId` é outro diff pequeno.
Dois commits limpos.

### Passo 2: Migração para walrus

Usar o código walrus já existente como referência para a lógica (ele funciona,
exceto para tipos recursivos). Reescrever a *forma* — mesma estrutura do
original, só trocando a API.

## Regras gerais

1. **Não remova comentários.** Comentários como `// len_a = a.len`, `// params`,
   `// locals` documentam a intenção e devem ser mantidos.

2. **Não mude nomes.** Mantenha os nomes originais de funções, campos, variáveis e
   parâmetros. Os tipos mudam (ex: `u32` → `TypeId`), mas os nomes ficam iguais:
   - Funções: `code_string_concat`, `code_composite_or_union_eq`, etc. (não
     renomear para `build_*`)
   - Campos: `global_index`, `type_index`, `index` (não mudar para `global_id`,
     `type_id`, `id`)
   - Variáveis: `instructions` (não mudar para `seq`)

3. **Não mude a ordem das funções.** Cada função deve ficar na mesma posição
   relativa no arquivo.

4. **Não faça inline de funções.** Se `code_start` era separada de
   `function_start`, mantenha separada. Se `code_eq` era separada de
   `function_eq`, mantenha separada. Se `code_inspect` era separada de
   `function_inspect`, mantenha separada.

5. **Não remova funções.** Se existia `code_variant_constructor`, ela deve
   continuar existindo (com a implementação walrus).

6. **Não crie variáveis desnecessárias.** Se um valor é usado apenas uma vez, use
   a expressão diretamente.

7. **Mantenha `let _ =` em chamadas de instrução.** O original usa
   `let _ = instructions.xxx()`. Mantenha esse padrão.

8. **Mantenha `panic!()` explícitos.** Não remova branches de panic. Não troque
   match arms por `_ =>`.

9. **Use `#[rustfmt::skip]` para manter formatação.** Quando o rustfmt quebraria
   uma linha que no original cabia em uma, use `#[rustfmt::skip]` antes do `let`
   para manter o formato original e reduzir o diff.

10. **Sem indexação.** Nunca use `param_ids[0]`. Crie cada parâmetro
    individualmente com nome descritivo:

    ```rust
    // ERRADO:
    let param_ids: Vec<LocalId> = params.iter().map(...).collect();
    let a = param_ids[0];

    // CERTO:
    let a = self.wasm_local(string_vt);
    let b = self.wasm_local(string_vt);
    ```

11. **Corrija todos os warnings do `cargo clippy`.** Não deve haver warnings
    novos.

## Modelo de locals do walrus

No walrus, **todos** os locals (parâmetros e variáveis locais de todas as funções)
são criados em um arena global: `wasm_module.locals.add(val_type) -> LocalId`.

O `FunctionBuilder::new(types, params, results)` define apenas o **tipo** da
função (assinatura). Ele não sabe nada sobre locals.

O `fb.finish(args, funcs)` recebe `args: Vec<LocalId>` — estes são os
**parâmetros** da função. Todos os outros `LocalId` que aparecem nas instruções
são automaticamente tratados como variáveis locais da função.

Isso significa que, no código:

```rust
let a = self.wasm_local(string_vt);  // param 0
let b = self.wasm_local(string_vt);  // param 1
let len_a = self.wasm_local(I32);    // variável local
let len_b = self.wasm_local(I32);    // variável local
```

Todos são criados no mesmo arena. O `finish(vec![a, b])` informa ao walrus que
`a` e `b` são parâmetros. O walrus descobre que `len_a` e `len_b` são variáveis
locais porque foram usados nas instruções mas não estão em `args`.

## Arquitetura walrus

### Helper `wasm_local`

Atalho para criar locals no módulo walrus:

```rust
impl Generator {
    fn wasm_local(
        &mut self,
        val_type: ValType,
    ) -> LocalId {
        self.wasm_module.locals.add(val_type)
    }
}
```

### API fluida: `FnInstructions`

O tipo `FnInstructions` encapsula o `FunctionBuilder` do walrus e uma referência
`Option` ao `Generator`:

```rust
struct FnInstructions<'a> {
    gen: Option<&'a mut Generator<'a>>,
    fb: FunctionBuilder,
    params: Vec<LocalId>,
    int: IntType,
    float: FloatType,
    string: StringType,
}
```

O `FnInstructions` tem:
- **Métodos de instrução** (`local_get`, `i32_const`, `string_len`, etc.) que
  delegam para `self.fb.func_body()`
- **`rust(|gen, s|)`** para os raros casos que precisam de acesso ao Generator
  intercalado com instruções (ex: `function_start` que itera sobre `self.consts`)
- **`if_`, `if_else`, `block_`, `loop_`** com closures para controle de fluxo
- **`finish()`** que consome o builder e devolve `FunctionId`

As closures de controle de fluxo recebem um builder de sub-bloco com os mesmos
métodos de instrução (mas sem `gen` — valores dependentes do Generator devem ser
computados **antes** de chamar `self.function_builder()`).

### Criação via `self.function_builder()`

Recebe os `LocalId` dos parâmetros (já criados antes) e os tipos dos resultados.
Os `ValType` dos parâmetros são recuperados internamente a partir dos `LocalId`:

```rust
impl Generator {
    fn function_builder(
        &mut self,
        name: &str,
        params: &[LocalId],
        results: &[ValType],
    ) -> FnInstructions {
        let int = self.int;
        let float = self.float;
        let string = self.string;
        let param_tys: Vec<ValType> = params
            .iter()
            .map(|&id| {
                self.wasm_module.locals.get(id).ty()
            })
            .collect();
        let mut fb = FunctionBuilder::new(
            &mut self.wasm_module.types,
            &param_tys,
            results,
        );
        fb.name(name.to_string());
        FnInstructions {
            gen: Some(self),
            fb,
            params: params.to_vec(),
            int,
            float,
            string,
        }
    }
}
```

### `rust()` para acesso ao Generator

Raramente necessário. A maioria dos valores dependentes do Generator
(`find_global_expect`, `ok_variant_constructor`, `val_type`, etc.) pode ser
computada **antes** de `self.function_builder()`. O `rust()` é reservado para
casos como `function_start` que iteram sobre `self.consts` e intercalam acesso ao
Generator com emissão de instruções.

Usa o truque do `Option` para separar `gen` e `instructions`:

```rust
impl FnInstructions {
    fn rust(
        &mut self,
        f: impl FnOnce(&mut Generator, &mut Self),
    ) -> &mut Self {
        let gen =
            self.gen.take().expect("gen available");
        f(gen, self);
        self.gen = Some(gen);
        self
    }
}
```

### `finish()` consome e devolve `FunctionId`

Passa os `params` (armazenados na criação) ao walrus. O walrus descobre quais
outros `LocalId` usados nas instruções são variáveis locais:

```rust
impl FnInstructions {
    fn finish(mut self) -> FunctionId {
        let gen =
            self.gen.take().expect("gen available");
        self.fb.finish(
            self.params,
            &mut gen.wasm_module.funcs,
        )
    }
}
```

### Exemplos

**Função simples (code_i32_to_int):**

```rust
pub(super) fn code_i32_to_int(
    &mut self,
) -> FunctionId {
    let param = self.wasm_local(ValType::I32);
    self.function_builder(
        "_i32_to_int",
        &[param],
        &[self.int.val_type()],
    )
    .local_get(param)
    .i32_to_int()
    .finish()
}
```

**Função com valores do Generator computados antes (code_int_repr):**

```rust
pub(super) fn code_int_repr(
    &mut self,
) -> FunctionId {
    let value = self.wasm_local(self.int.val_type());
    let ptr = self.wasm_local(ValType::I32);
    let to_str = match self.int {
        IntType::I32 => {
            self.find_global_expect(I32_TO_STR)
        }
        IntType::I64 => {
            self.find_global_expect(I64_TO_STR)
        }
    };
    self.function_builder(
        "_repr_int",
        &[value, ptr],
        &[ValType::I32],
    )
    .local_get(value)
    .local_get(ptr)
    .call(to_str.func_id())
    .finish()
}
```

**Função com if_else (code_string_get_byte):**

```rust
pub(super) fn code_string_get_byte(
    &mut self,
) -> FunctionId {
    let result_vt = self.val_type(
        &type_::result(type_::int(), type_::nil()),
    );
    // params
    let s = self.wasm_local(self.string.val_type());
    let i = self.wasm_local(self.int.val_type());
    // values computed before function_builder
    let ok_fn = self.ok_variant_constructor(
        type_::int(),
        type_::nil(),
    );
    let error_fn = self.error_variant_constructor(
        type_::int(),
        type_::nil(),
    );
    self.function_builder(
        "_string_get_byte",
        &[s, i],
        &[result_vt],
    )
    .local_get(i)
    .local_get(s)
    .string_len()
    .i32_to_int()
    .int_lt()
    .local_get(i)
    .int_const(&0.into())
    .int_ge()
    .i32_and()
    .if_else(
        result_vt,
        |then_s| {
            let _ = then_s
                .local_get(s)
                .local_get(i)
                .int_to_i32()
                .string_get()
                .i32_to_int()
                .call(ok_fn);
        },
        |else_s| {
            let _ = else_s
                .nil_const()
                .call(error_fn);
        },
    )
    .finish()
}
```

**Função com loop e variáveis locais (code_string_to_memory):**

Note que `len`, `i`, `needed` são variáveis locais criadas com `wasm_local` mas
NÃO passadas como parâmetros em `function_builder`. O walrus as descobre
automaticamente quando aparecem nas instruções.

```rust
pub(super) fn code_string_to_memory(
    &mut self,
) -> FunctionId {
    // params
    let s = self.wasm_local(self.string.val_type());
    let dest = self.wasm_local(ValType::I32);
    // locals (not params — not passed to function_builder)
    let len = self.wasm_local(ValType::I32);
    let i = self.wasm_local(ValType::I32);
    let needed = self.wasm_local(ValType::I32);
    let memory =
        self.memory.expect("memory for builtins");
    self.function_builder(
        "_string_to_memory",
        &[s, dest],
        &[ValType::I32],
    )
    .local_get(s)
    .array_len()
    .local_set(len)
    .local_get(dest)
    .local_get(len)
    .i32_add()
    .ensure_memory(needed, memory)
    .i32_const(0)
    .local_set(i)
    .loop_(None, |loop_s| {
        let loop_id = loop_s.id();
        let _ = loop_s
            .local_get(i)
            .local_get(len)
            .i32_lt_u()
            .if_(None, |body| {
                let _ = body
                    .local_get(dest)
                    .local_get(s)
                    .local_get(i)
                    .string_get()
                    .i32_store8(memory)
                    .i32_inc(dest)
                    .i32_inc(i)
                    .br(loop_id);
            });
    })
    .local_get(len)
    .finish()
}
```

**Função com `rust()` (function_start — caso raro):**

```rust
// Exemplo: function_start itera sobre self.consts
// e precisa intercalar acesso ao Generator com instruções
let _ = instructions.rust(|gen, s| {
    for const_ in gen.consts.clone() {
        match const_ {
            WasmConst::String { dest, src } => {
                let _ = s
                    .global_get(src)
                    .global_set(dest);
            }
            WasmConst::Var {
                global_index,
                name,
            } => {
                let src =
                    gen.find_global_expect(&name);
                let _ = s
                    .global_get(src.global_id())
                    .global_set(global_index);
            }
            // ...
        }
    }
});
```

### Generator

- `Generator` ganha um campo `pub(super) wasm_module: walrus::Module`
  inicializado com `walrus::Module::from_buffer(BUILTINS_WASM)` no `new()`.
- Campos que usavam `u32` passam a usar tipos walrus: `TypeId`, `FunctionId`,
  `GlobalId`, `LocalId`, `MemoryId`. **Mas mantêm os nomes originais**
  (`type_index`, `global_index`, etc.).
- `WasmFunction` struct e `BTreeSet<WasmFunction>` são removidos — walrus
  gerencia funções diretamente.
- `global_section`, `import_section`, `export_section`, `data_section` são
  removidos — walrus gerencia seções.
- Adicionar campo `memory: Option<MemoryId>`.

### IdKind e Id (scope.rs)

**Manter a estrutura original** com `IdKind` enum e `Id` struct:

```rust
enum IdKind {
    Global,
    Func,
    Local,
}

struct Id {
    pub kind: IdKind,
    pub name: EcoString,
    // O index agora precisa armazenar GlobalId,
    // FunctionId ou LocalId.
    // Usar um enum interno ou similar.
}
```

### get_function_builtin_external (webassembly.rs)

Manter como dispatch simples para `code_*`:

```rust
fn get_function_builtin_external(
    &mut self,
    builtin: BuiltinFunctionExternal,
) -> FunctionId {
    if let Some(id) =
        self.builtins_external.get(&builtin)
    {
        return *id;
    }
    match builtin {
        BuiltinFunctionExternal::StringConcat => {
            self.code_string_concat()
        }
        BuiltinFunctionExternal::StringNumBytes => {
            self.code_string_num_bytes()
        }
        // ... etc (todos os arms explícitos, sem _ =>)
    }
}
```

### compile()

Simplificado porque walrus gerencia seções:

```rust
fn compile(
    mut self,
) -> Result<Vec<u8>, Error> {
    let start = self.generate()?;
    self.wasm_module.name =
        Some(self.module.name.to_string());
    self.wasm_module.start = Some(start);
    // Element segments para funções declaradas
    // ...
    // Dead code elimination
    eliminate_dead_code(
        &mut self.wasm_module,
        &self.builtin_data_names,
    );
    Ok(self.wasm_module.emit_wasm())
}
```

### eliminate_dead_code

Manter como **função standalone** (não método):

```rust
fn eliminate_dead_code(
    module: &mut walrus::Module,
    builtin_data_names: &[String],
) {
    // Corpo praticamente idêntico ao original,
    // mas sem from_buffer/emit_wasm
}
```

### native.rs

Remover `native.rs` — `walrus::Module::from_buffer` substitui `parse_builtins`.

## Controle de fluxo — padrão de migração

Original (wasm-encoder):

```rust
#[rustfmt::skip]
let _ = instructions
    .expression(self, locals, scope.clone(), left)
    .if_(BlockType::Result(BOOL_VALTYPE))
      .expression(self, locals, scope, right)
    .else_()
      .bool_const(false)
    .end();
```

Walrus (com closures):

```rust
let _ = instructions
    .expression(self, locals, scope.clone(), left)
    .if_else(
        BOOL_VALTYPE,
        |then_s| {
            let _ = then_s.expression(
                self, locals, scope, right,
            );
        },
        |else_s| {
            let _ = else_s.bool_const(false);
        },
    );
```

Para loops:

```rust
// Original:
let _ = instructions.loop_(BlockType::Empty);
let _ = instructions.xxx().br(0);
let _ = instructions.end();

// Walrus:
let _ = instructions.loop_(None, |loop_s| {
    let loop_id = loop_s.id();
    let _ = loop_s.xxx().br(loop_id);
});
```

## Verificação

Após cada mudança significativa, rodar:

```bash
cargo build -p gleam-core
cargo fmt -p gleam-core
cargo fmt --check -p gleam-core
cargo clippy -p gleam-core
cargo test -p gleam-core webassembly::tests
```

Os 5 testes com stack overflow (`tree_operations`, `mutual_types`,
`echo_various_types`, `list_pattern_guard_field_access`,
`import_generic_recursive_function`) são problemas conhecidos — ignorar.

## Medição do diff

Usar sempre:

```bash
git diff 60a17fe97 --numstat -- \
    compiler-core/src/webassembly.rs \
    compiler-core/src/webassembly/builtins.rs \
    compiler-core/src/webassembly/instructions.rs \
    compiler-core/src/webassembly/scope.rs \
    compiler-core/src/webassembly/native.rs
```

Nota: `git diff 60a17fe97` (sem `..HEAD`) compara o baseline com o working tree.
