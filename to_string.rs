    fn function_external_generic(
        &mut self,
        module: &str,
        name: &str,
        required_type: &Arc<Type>,
        function: &TypedFunction,
    ) -> Id {
        let (params, _) = required_type.fn_types().unwrap();
        let fname = mangle(function_name(function), required_type);
        if params[0].is_int() {
            let param = 0;
            let len = 1;
            let i = 2;
            let r = 3;
            let mut code = Function::new(vec![(2, ValType::I32), (1, self.string.val_type())]);
            let mut instructions = code.instructions();
            #[rustfmt::skip]
            let _ = instructions
                .local_get(param)
                .i32_const(1) // dest address
                .call(todo!())
                .local_tee(len)
                .array_new_default(self.string.type_index)
                .local_set(r)
                .i32_const(0)
                .local_set(i)
                .loop_(BlockType::Empty)
                  .local_get(i)
                  .local_get(len)
                  .i32_lt_u()
                  .if_(BlockType::Empty)
                    .local_get(r)
                    .local_get(i)
                    .local_get(i)
                    .i32_load8_u(MemArg { offset: 1, align: 0, memory_index: 0 })
                    .array_set(self.string.type_index)
                    .local_get(i)
                    .i32_const(1)
                    .i32_add()
                    .local_set(i)
                    .br(1) // loop
                  // if
                  .end()
                // loop
                .end()
                .local_get(r)
                // function
                .end();
            let index = self.add_function(
                None,
                vec![ValType::I32],
                vec![self.string.val_type()],
                code.into_raw_body(),
            );
            return Id {
                kind: IdKind::Func,
                name: fname,
                index,
            };
        }
        panic!();
    }
