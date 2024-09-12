use ecow::EcoString;

use std::{collections::HashMap, sync::Arc};

use super::table::{ConstantId, FunctionId, LocalId, ProductId};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Binding {
    Function(FunctionId),
    Local(LocalId),
    Constant(ConstantId),
    Product(ProductId),
}

#[derive(Clone, Default, Debug)]
pub struct Scope {
    pub(crate) bindings: HashMap<EcoString, Binding>,
    pub(crate) enclosing: Option<Arc<Scope>>,
}

impl Scope {
    pub(crate) fn new() -> Arc<Self> {
        Arc::new(Self {
            bindings: HashMap::new(),
            enclosing: None,
        })
    }

    pub(crate) fn new_enclosing(enclosing: Arc<Self>) -> Arc<Self> {
        Arc::new(Self {
            bindings: HashMap::new(),
            enclosing: Some(enclosing),
        })
    }

    pub(crate) fn set(&self, name: &str, binding: Binding) -> Arc<Self> {
        let mut new_env = self.clone();
        let _ = new_env.bindings.insert(name.into(), binding);
        Arc::new(new_env)
    }

    pub(crate) fn get(&self, name: &str) -> Option<Binding> {
        if let Some(val) = self.bindings.get(name) {
            Some(*val)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(name)
        } else {
            None
        }
    }
}
