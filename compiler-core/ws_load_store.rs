fn code_memory_store(&self) -> Function {
        let mut function = Function::new(vec![]);
        // params
        let address = 0; // I32
        let value = 1; // I32
        // return I32
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .local_get(address)
            .local_get(value)
            .i32_store(MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            })
            .local_get(value)
            .end();
        function
    }

    fn code_memory_load(&self) -> Function {
        let mut function = Function::new(vec![]);
        // params
        let address = 0; // I32
        // return I32
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .local_get(address)
            .i32_load(MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            })
            .end();
        function
    }
