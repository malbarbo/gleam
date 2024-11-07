use ecow::EcoString;

use super::{
    encoder::{WasmFunction, WasmInstructions, WasmTypeImpl},
    table::{self, FunctionId, LocalStore, SymbolTable, TypeId},
};

pub fn emit_string_equality_function(
    function_type: TypeId,
    function_id: FunctionId,
    table: &SymbolTable,
) -> WasmFunction {
    /*
       (func $string_equality (param $string $string) (result $string)
           (local $counter i32)
           (block
               (; Check if strings are the same length ;)
               (local.get 0)
               (array.len)
               (local.tee $counter)
               (local.get 1)
               (array.len)
               (i32.eq)
               (i32.eqz)
               (br_if 0)

               (; Loop through each byte ;)
               (loop
                   (; if counter == 0, then strings are equal ;)
                   (local.get $counter)
                   (i32.eqz)
                   (br_if 0)

                   (; decrement counter ;)
                   (local.get $counter)
                   (i32.const 1)
                   (i32.sub)
                   (local.set $counter)

                   (; if string1[counter] != string2[counter], then strings are not equal ;)
                   (local.get 0)
                   (local.get $counter)
                   (array.get_u)
                   (local.get 1)
                   (local.get $counter)
                   (array.get_u)
                   (i32.eq)
                   (i32.eqz)
                   (br_if 1)
               )
               (i32.const 1)
               (return)
           )
           (i32.const 0)
       )
    */

    // variables
    let mut local_generator = LocalStore::with_offset(2);
    let counter_id = local_generator.new_id();
    let counter = table::Local {
        id: counter_id,
        name: "counter".into(),
        wasm_type: WasmTypeImpl::Int,
    };
    local_generator.insert(counter_id, counter);

    // code
    let mut i = vec![];
    let string_type_id = table
        .types
        .get(table.string_type.unwrap())
        .unwrap()
        .definition
        .id;
    {
        use wasm_encoder::Instruction::*;

        i.push(Block(wasm_encoder::BlockType::Empty));
        {
            i.push(LocalGet(0));
            i.push(ArrayLen);
            i.push(LocalTee(counter_id.id()));
            i.push(LocalGet(1));
            i.push(ArrayLen);
            i.push(I32Eq);
            i.push(I32Eqz);
            i.push(BrIf(0));

            i.push(Loop(wasm_encoder::BlockType::Empty));
            {
                i.push(LocalGet(counter_id.id()));
                i.push(I32Eqz);
                i.push(BrIf(0));

                i.push(LocalGet(counter_id.id()));
                i.push(I32Const(1));
                i.push(I32Sub);
                i.push(LocalSet(counter_id.id()));

                i.push(LocalGet(0));
                i.push(LocalGet(counter_id.id()));
                i.push(ArrayGetU(string_type_id));
                i.push(LocalGet(1));
                i.push(LocalGet(counter_id.id()));
                i.push(ArrayGetU(string_type_id));
                i.push(I32Eq);
                i.push(I32Eqz);
                i.push(BrIf(1));
            }
            i.push(End);
            i.push(I32Const(1));
            i.push(Return);
        }
        i.push(End);
        i.push(I32Const(0));
        i.push(End);
    }

    WasmFunction {
        name: "string_equality".into(),
        function_index: function_id.id(),
        type_index: table.types.get(function_type).unwrap().definition.id,
        arity: 2,
        instructions: WasmInstructions { lst: i },
        locals: local_generator
            .as_list()
            .into_iter()
            .map(|x| (x.name, x.wasm_type))
            .collect(),
        argument_names: [Some("lhs"), Some("rhs")]
            .into_iter()
            .map(|x| x.map(EcoString::from))
            .collect(),
        public: false,
    }
}
