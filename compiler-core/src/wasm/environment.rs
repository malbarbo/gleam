use std::collections::HashMap;

use super::table::{ConstantId, FunctionId, LocalId, ProductId, SumId};
use ecow::EcoString;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    Local(LocalId),
    Function(FunctionId),
    Product(ProductId),
    Sum(SumId),
    Constant(ConstantId),
    Builtin(BuiltinType),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Nil,
    Boolean { value: bool },
}

pub struct Environment<'a> {
    /// The values of the bindings in the current scope.
    values: HashMap<EcoString, Binding>,

    /// The parent scope, if any.
    enclosing: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn with_enclosing(enclosing: &'a Environment<'a>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn set(&mut self, name: EcoString, binding: Binding) {
        _ = self.values.insert(name, binding);
    }

    pub fn get(&self, name: &str) -> Option<Binding> {
        self.values.get(name).cloned().or_else(|| {
            self.enclosing
                .as_ref()
                .and_then(|enclosing| enclosing.get(name))
        })
    }
}
