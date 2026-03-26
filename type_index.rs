fn type_index(&mut self, type_: &Arc<Type>) -> Option<u32> {
        if let Some(item_type) = type_.list_type() {
            Some(self.list_type_index(&item_type))
        } else if let Some(types) = type_.tuple_types() {
            Some(self.tuple_type_index(types))
        } else if let Some((params, return_)) = type_.fn_types() {
            Some(self.function_type_index(params, Some(return_)))
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            match custom_type {
                CustomType::ExternalI32 | CustomType::Enum { .. } => None,
                CustomType::Struct {
                    custom_type,
                    constructor,
                } => {
                    let (type_index, _) =
                        self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                    Some(type_index)
                }
                CustomType::Union { custom_type } => {
                    if let Some(constructor) = custom_type_inferred_constructor(&custom_type, type_)
                    {
                        let (_, type_index, _) =
                            self.mono_union_subtype_index(type_, &custom_type, constructor, &args);
                        Some(type_index)
                    } else {
                        Some(self.mono_union_supertype_index(type_, &custom_type))
                    }
                }
            }
        } else {
            None
        }
    }
