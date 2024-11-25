use ecow::EcoString;
use wasm_encoder::MemArg;

use super::{
    encoder::{WasmFunction, WasmInstructions, WasmTypeImpl},
    table::{self, FunctionId, LocalStore, SymbolTable, TypeId},
};

pub fn emit_streq(
    function_type: TypeId,
    function_id: FunctionId,
    table: &SymbolTable,
) -> WasmFunction {
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
    let lhs_id = 0;
    let rhs_id = 1;
    let mut i = vec![];
    let string_type_id = table
        .types
        .get(table.string_type.unwrap())
        .unwrap()
        .definition
        .id;
    {
        use wasm_encoder::Instruction::*;

        {
            i.push(Block(wasm_encoder::BlockType::Empty));

            // Check if strings are the same length
            i.push(LocalGet(lhs_id));
            i.push(ArrayLen);
            i.push(LocalTee(counter_id.id()));
            i.push(LocalGet(rhs_id));
            i.push(ArrayLen);
            i.push(I32Eq);
            i.push(I32Eqz);
            i.push(BrIf(0));

            {
                i.push(Block(wasm_encoder::BlockType::Empty));
                {
                    i.push(Loop(wasm_encoder::BlockType::Empty));
                    i.push(LocalGet(counter_id.id()));
                    i.push(I32Eqz);
                    i.push(BrIf(1)); // loop creates a branch at beginning, not at end

                    i.push(LocalGet(counter_id.id()));
                    i.push(I32Const(1));
                    i.push(I32Sub);
                    i.push(LocalSet(counter_id.id()));

                    i.push(LocalGet(lhs_id));
                    i.push(LocalGet(counter_id.id()));
                    i.push(ArrayGetU(string_type_id));
                    i.push(LocalGet(rhs_id));
                    i.push(LocalGet(counter_id.id()));
                    i.push(ArrayGetU(string_type_id));
                    i.push(I32Eq);
                    i.push(I32Eqz);
                    i.push(BrIf(2)); // quit loop

                    i.push(Br(0));
                    i.push(End);
                }
                i.push(End);
            }
            i.push(I32Const(1));
            i.push(Return);
            i.push(End);
        }
        i.push(I32Const(0));
        i.push(End);
    }

    WasmFunction {
        name: "streq".into(),
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

pub fn emit_strcat(
    function_type: TypeId,
    function_id: FunctionId,
    table: &SymbolTable,
) -> WasmFunction {
    let string_type_id = table
        .types
        .get(table.string_type.unwrap())
        .unwrap()
        .definition
        .id;

    // variables
    let lhs = 0;
    let rhs = 1;
    let mut local_generator = LocalStore::with_offset(2);
    let lhs_len_id = local_generator.new_id();
    let lhs_len = table::Local {
        id: lhs_len_id,
        name: "lhs_len".into(),
        wasm_type: WasmTypeImpl::Int,
    };
    local_generator.insert(lhs_len_id, lhs_len);

    let rhs_len_id = local_generator.new_id();
    let rhs_len = table::Local {
        id: rhs_len_id,
        name: "rhs_len".into(),
        wasm_type: WasmTypeImpl::Int,
    };
    local_generator.insert(rhs_len_id, rhs_len);

    let result_id = local_generator.new_id();
    let result = table::Local {
        id: result_id,
        name: "new_string".into(),
        wasm_type: WasmTypeImpl::ArrayRef(string_type_id),
    };
    local_generator.insert(result_id, result);

    // code
    let mut i = vec![];

    {
        use wasm_encoder::Instruction::*;

        i.push(LocalGet(lhs));
        i.push(ArrayLen);
        i.push(LocalTee(lhs_len_id.id()));

        i.push(LocalGet(rhs));
        i.push(ArrayLen);
        i.push(LocalTee(rhs_len_id.id()));

        i.push(I32Add);
        i.push(ArrayNewDefault(string_type_id));
        i.push(LocalSet(result_id.id()));

        // destination, start, origin, start, length
        i.push(LocalGet(result_id.id()));
        i.push(I32Const(0));
        i.push(LocalGet(lhs));
        i.push(I32Const(0));
        i.push(LocalGet(lhs_len_id.id()));
        i.push(ArrayCopy {
            array_type_index_src: string_type_id,
            array_type_index_dst: string_type_id,
        });

        // destination, start, origin, start, length
        i.push(LocalGet(result_id.id()));
        i.push(LocalGet(lhs_len_id.id()));
        i.push(LocalGet(rhs));
        i.push(I32Const(0));
        i.push(LocalGet(rhs_len_id.id()));
        i.push(ArrayCopy {
            array_type_index_src: string_type_id,
            array_type_index_dst: string_type_id,
        });

        i.push(LocalGet(result_id.id()));
        i.push(End);
    }

    WasmFunction {
        name: "strcat".into(),
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

// strsub(str, start, end) -> str
// Returns a substring of a string from a start index to an end index. end index is not inclusive.
// If start or end are negative, they will be treated as offsets from the string length + 1.
// If start is < 0, it will be treated as 0. If end is > #str, it will be treated as #str.
// If after this start is greater than or equal to end, the function will return an empty string.
pub fn emit_strsub(
    function_type: TypeId,
    function_id: FunctionId,
    table: &SymbolTable,
) -> WasmFunction {
    /*
       (func $strsub (param ($str (ref $String)) ($start i32) ($end i32)) (result ($ref String))
           (local $str_len i32)
           (local $result (ref $String))
           (local $result_len i32)

           (local.get $str)
           (array.len)
           (local.set $str_len)

           ; if start is negative, replace by length + start
           (local.get $start)
           (const.i32 0)
           (i32.lt_s)
           (if
               (local.get $str_len)
               (local.get $start)
               (i32.add)
               (local.set $start)
           end)

           ; if end is negative, replace by length + end + 1
           (local.get $end)
           (const.i32 0)
           (i32.lt_s)
           (if
               (local.get $str_len)
               (local.get $end)
               (i32.add)
               (local.set $end)
           end)

           ; if start < 0, replace by 0
           (local.get $start)
           (const.i32 0)
           (i32.lt_s)
           (if
               (const.i32 0)
               (local.set $start)
           end)

           ; if end > length, replace by length
           (local.get $end)
           (local.get $str_len)
           (i32.gt_s)
           (if
               (local.get $str_len)
               (local.set $end)
           end)

           ; if start >= end, return empty string
           (local.get $start)
           (local.get $end)
           (i32.ge_s)
           (if
               (i32.const 0)
               (array.new $String)
               (return)
           end)

           ; otherwise, return substring
           ; length
           (local.get $end)
           (local.get $start)
           (i32.sub)
           (local.tee $result_len)
           (array.new_default $String)
           (local.set $result)

           ; origin, start, destination, start, length
           (local.get $str)
           (local.get $start)
           (local.get $result)
           (i32.const 0)
           (local.get $result_len)
           (array.copy $String $String)
       )

    */
    let string_type_id = table
        .types
        .get(table.string_type.unwrap())
        .unwrap()
        .definition
        .id;

    // variables
    let str_id = 0;
    let start_id = 1;
    let end_id = 2;

    let mut local_generator = LocalStore::with_offset(3);
    let str_len_id = local_generator.new_id();
    let str_len = table::Local {
        id: str_len_id,
        name: "str_len".into(),
        wasm_type: WasmTypeImpl::Int,
    };
    local_generator.insert(str_len_id, str_len);

    let result_id = local_generator.new_id();
    let result = table::Local {
        id: result_id,
        name: "new_string".into(),
        wasm_type: WasmTypeImpl::ArrayRef(string_type_id),
    };
    local_generator.insert(result_id, result);

    let result_len_id = local_generator.new_id();
    let result_len = table::Local {
        id: result_len_id,
        name: "result_len".into(),
        wasm_type: WasmTypeImpl::Int,
    };
    local_generator.insert(result_len_id, result_len);

    // code
    let mut i = vec![];

    {
        use wasm_encoder::Instruction::*;
        i.push(LocalGet(str_id));
        i.push(ArrayLen);
        i.push(LocalSet(str_len_id.id()));

        // if start is negative, replace by length + start
        i.push(LocalGet(start_id));
        i.push(I32Const(0));
        i.push(I32LtS);
        i.push(If(wasm_encoder::BlockType::Empty));
        {
            i.push(LocalGet(str_len_id.id()));
            i.push(LocalGet(start_id));
            i.push(I32Add);
            i.push(LocalSet(start_id));
        }
        i.push(End);

        // if end is negative, replace by length + end + 1
        i.push(LocalGet(end_id));
        i.push(I32Const(0));
        i.push(I32LtS);
        i.push(If(wasm_encoder::BlockType::Empty));
        {
            i.push(LocalGet(str_len_id.id()));
            i.push(LocalGet(end_id));
            i.push(I32Add);
            i.push(I32Const(1));
            i.push(I32Add);
            i.push(LocalSet(end_id));
        }
        i.push(End);

        // if start < 0, replace by 0
        i.push(LocalGet(start_id));
        i.push(I32Const(0));
        i.push(I32LtS);
        i.push(If(wasm_encoder::BlockType::Empty));
        {
            i.push(I32Const(0));
            i.push(LocalSet(start_id));
        }
        i.push(End);

        // if end > length, replace by length
        i.push(LocalGet(end_id));
        i.push(LocalGet(str_len_id.id()));
        i.push(I32GtS);
        i.push(If(wasm_encoder::BlockType::Empty));
        {
            i.push(LocalGet(str_len_id.id()));
            i.push(LocalSet(end_id));
        }
        i.push(End);

        // if start >= end, return empty string
        i.push(LocalGet(start_id));
        i.push(LocalGet(end_id));
        i.push(I32GeS);
        i.push(If(wasm_encoder::BlockType::Empty));
        {
            i.push(I32Const(0));
            i.push(ArrayNewDefault(string_type_id));
            i.push(Return);
        }
        i.push(End);

        // otherwise, return substring
        // length
        i.push(LocalGet(end_id));
        i.push(LocalGet(start_id));
        i.push(I32Sub);
        i.push(LocalTee(result_len_id.id()));
        i.push(ArrayNewDefault(string_type_id));
        i.push(LocalSet(result_id.id()));

        // destination, start, origin, start, length
        i.push(LocalGet(result_id.id()));
        i.push(I32Const(0));
        i.push(LocalGet(str_id));
        i.push(LocalGet(start_id));
        i.push(LocalGet(result_len_id.id()));
        i.push(ArrayCopy {
            array_type_index_src: string_type_id,
            array_type_index_dst: string_type_id,
        });
        i.push(LocalGet(result_id.id()));

        // end
        i.push(End);
    }

    WasmFunction {
        name: "strsub".into(),
        function_index: function_id.id(),
        type_index: table.types.get(function_type).unwrap().definition.id,
        arity: 3,
        instructions: WasmInstructions { lst: i },
        locals: local_generator
            .as_list()
            .into_iter()
            .map(|x| (x.name, x.wasm_type))
            .collect(),
        argument_names: [Some("str"), Some("start"), Some("end")]
            .into_iter()
            .map(|x| x.map(EcoString::from))
            .collect(),
        public: false,
    }
}

pub fn emit_strwritestdout(
    function_type: TypeId,
    function_id: FunctionId,
    table: &SymbolTable,
) -> WasmFunction {
    // variables
    let string_type_id = table
        .types
        .get(table.string_type.unwrap())
        .unwrap()
        .definition
        .id;
    let str_id = 0;

    let mut local_generator = LocalStore::with_offset(1);
    let str_len_id = local_generator.new_id();
    let str_len = table::Local {
        id: str_len_id,
        name: "str_len".into(),
        wasm_type: WasmTypeImpl::Int,
    };
    local_generator.insert(str_len_id, str_len);

    let offset_id = local_generator.new_id();
    let offset = table::Local {
        id: offset_id,
        name: "offset".into(),
        wasm_type: WasmTypeImpl::Int,
    };
    local_generator.insert(offset_id, offset);

    // code
    let mut i = vec![];

    {
        use wasm_encoder::Instruction::*;

        // write iov_base
        // pointer to start of string (position 4, length 4)
        i.push(I32Const(4));
        i.push(I32Const(12));
        i.push(I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));

        // write iov_len
        // length of string (position 8, length 4)
        i.push(I32Const(8));
        i.push(LocalGet(str_id));
        i.push(ArrayLen);
        i.push(I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));

        // write the string itself to memory
        // ugh... need to loop through the string...
        // while string_length > 0, write 1 byte of the string, then increment the pointer and decrement the string_length
        i.push(LocalGet(str_id));
        i.push(ArrayLen);
        i.push(LocalSet(str_len_id.id()));

        i.push(I32Const(0));
        i.push(LocalSet(offset_id.id()));

        i.push(Block(wasm_encoder::BlockType::Empty));
        {
            i.push(Loop(wasm_encoder::BlockType::Empty));
            {
                i.push(LocalGet(str_len_id.id()));
                i.push(I32Eqz);
                i.push(BrIf(1));

                i.push(LocalGet(offset_id.id()));
                i.push(LocalGet(str_id));
                i.push(LocalGet(offset_id.id()));
                i.push(ArrayGetU(string_type_id));
                i.push(I32Store8(MemArg {
                    offset: 12,
                    align: 0,
                    memory_index: 0,
                }));

                i.push(LocalGet(offset_id.id()));
                i.push(I32Const(1));
                i.push(I32Add);
                i.push(LocalSet(offset_id.id()));

                i.push(LocalGet(str_len_id.id()));
                i.push(I32Const(1));
                i.push(I32Sub);
                i.push(LocalSet(str_len_id.id()));

                i.push(Br(0));
            }
            i.push(End);
        }
        i.push(End);

        // call fd_write
        i.push(I32Const(1)); // stdout
        i.push(I32Const(4)); // pointer to *iovs
        i.push(I32Const(1)); // number of iovs
        i.push(I32Const(0)); // where to write the number of bytes written
        i.push(Call(table.wasi_fd_write));

        // drop bytes written
        i.push(Drop);

        // push a nil
        i.push(I32Const(0));

        i.push(End);
    }

    WasmFunction {
        name: "strwritestdout".into(),
        function_index: function_id.id(),
        type_index: table.types.get(function_type).unwrap().definition.id,
        arity: 1,
        instructions: WasmInstructions { lst: i },
        locals: local_generator
            .as_list()
            .into_iter()
            .map(|x| (x.name, x.wasm_type))
            .collect(),
        argument_names: [Some("str")]
            .into_iter()
            .map(|x| x.map(EcoString::from))
            .collect(),
        public: false,
    }
}

// memtostr(mem, offset, length) -> str
// Copies a substring from memory into a new string from a given offset with a given length.

// strtomem(str, mem, offset) -> void
// Copies a string to memory at a given offset.

/*
TEMPLATE:

pub fn emit_strsub(
    function_type: TypeId,
    function_id: FunctionId,
    table: &SymbolTable,
) -> WasmFunction {
    let string_type_id = table
        .types
        .get(table.string_type.unwrap())
        .unwrap()
        .definition
        .id;

    // variables
    let mut local_generator = LocalStore::with_offset(2);

    // code
    let mut i = vec![];

    {
        use wasm_encoder::Instruction::*;
    }

    WasmFunction {
        name: "strcat".into(),
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

*/
