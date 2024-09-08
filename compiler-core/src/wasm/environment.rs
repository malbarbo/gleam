use crate::type_::Type;

use std::sync::Arc;

use ecow::EcoString;

use std::collections::HashMap;

#[derive(Clone, Default, Debug)]
pub struct Environment {
    pub(crate) bindings: HashMap<EcoString, (u32, Arc<Type>)>,
    pub(crate) enclosing: Option<Arc<Environment>>,
}

impl Environment {
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

    pub(crate) fn set(&self, name: &str, binding: u32, type_: Arc<Type>) -> Arc<Self> {
        let mut new_env = self.clone();
        let _ = new_env.bindings.insert(name.into(), (binding, type_));
        Arc::new(new_env)
    }

    pub(crate) fn get(&self, name: &str) -> Option<u32> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.0)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(name)
        } else {
            None
        }
    }
}
