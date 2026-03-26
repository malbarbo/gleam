// const STR_COMPARE: &str = "_str_compare";
// const STR_NUM_GRAPHEMES: &str = "_str_num_graphemes";
// const STR_GRAPHEMES_INDICES: &str = "_str_graphemes_indices";
// const STR_TO_LOWERCASE: &str = "_str_to_lowercase";
// const STR_TO_UPPERCASE: &str = "_str_to_uppercase";

    fn code_string_num_graphemes(&mut self) -> Function {
        let mut function = Function::new(vec![(2, ValType::I32)]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // String
        // locals
        let ptr = 1;
        let len = 2;
        // return Int
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let num_graphemes = self.find_global_expect(STR_NUM_GRAPHEMES);
        let heap_base = self.find_global_expect(HEAP_BASE);
        let _ = instructions
            .local_get(a)
            .call(heap_base.index)
            .local_tee(ptr)
            .call(string_to_memory)
            .local_set(len)
            .local_get(ptr)
            .local_get(len)
            .call(num_graphemes.index)
            .i32_to_int()
            .end();
        function
    }
