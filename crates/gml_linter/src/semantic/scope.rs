use rustc_hash::FxHashMap;
use super::types::Type;
use std::rc::Rc;

/// Tracks variable types within a scope (function or block).
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    variables: FxHashMap<String, Type>,
    parent: Option<Rc<TypeEnv>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    pub fn lookup(&self, name: &str) -> Option<Type> {
        if let Some(ty) = self.variables.get(name) {
            return Some(ty.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.lookup(name);
        }
        None
    }
    
    /// Create a child scope. Uses Rc for O(1) sharing instead of deep clone.
    pub fn fork(&self) -> Self {
        Self {
            variables: FxHashMap::default(),
            parent: Some(Rc::new(self.shallow_clone())),
        }
    }
    
    /// Shallow clone - only clones the variables map, not the parent chain
    fn shallow_clone(&self) -> Self {
        Self {
            variables: self.variables.clone(),
            parent: self.parent.clone(), // Rc::clone is O(1)
        }
    }

    /// Check if a variable was narrowed from Type::Any in a parent scope.
    pub fn is_narrowed_from_any(&self, name: &str) -> bool {
        if self.variables.contains_key(name) {
            if let Some(parent) = &self.parent {
                if let Some(parent_ty) = parent.lookup(name) {
                    if matches!(parent_ty, Type::Any) {
                        return true;
                    }
                }
            }
        }
        
        if let Some(parent) = &self.parent {
            return parent.is_narrowed_from_any(name);
        }
        false
    }
}

