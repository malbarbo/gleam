use ecow::EcoString;

use std::{collections::HashMap, sync::Arc};

#[derive(Clone, Default, Debug)]
pub struct Scope {
    pub(crate) bindings: HashMap<EcoString, u32>,
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

    pub(crate) fn set(&self, name: &str, binding: u32) -> Arc<Self> {
        let mut new_env = self.clone();
        let _ = new_env.bindings.insert(name.into(), binding);
        Arc::new(new_env)
    }

    pub(crate) fn get(&self, name: &str) -> Option<u32> {
        if let Some(val) = self.bindings.get(name) {
            Some(*val)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(name)
        } else {
            None
        }
    }
}
