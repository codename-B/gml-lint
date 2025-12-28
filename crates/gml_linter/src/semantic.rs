//! Semantic model for tracking bindings and references during AST traversal
//!
//! This module provides the SemanticModel which collects variable declarations (bindings)
//! and their usages (references) during a single-pass AST traversal. Rules can then
//! query this model to implement checks like "unused variable" or "uninitialized variable".
//!
//! Performance optimizations:
//! - Uses FxHashMap (rustc-hash) for faster hashing than std HashMap
//! - Pre-allocates vectors with estimated capacity
//! - Uses SmallVec for scope bindings (typically small number per scope)

use gml_lexer::Span;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

/// Unique identifier for a binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindingId(u32);

impl BindingId {
    #[inline]
    pub fn new(id: u32) -> Self {
        Self(id)
    }
    
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// Unique identifier for a scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

impl ScopeId {
    #[inline]
    pub fn new(id: u32) -> Self {
        Self(id)
    }
    
    #[inline]
    pub fn global() -> Self {
        Self(0)
    }
    
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// The kind of binding (how the variable was declared)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    /// Variable declaration: `var x = 5`
    Variable,
    /// Variable declared without initializer: `var x`
    UninitializedVariable,
    /// Function parameter
    Parameter,
    /// Function declaration
    Function,
    /// Global variable
    GlobalVar,
    /// Catch variable in try-catch
    CatchVar,
}

/// A binding represents a variable/function declaration
#[derive(Debug, Clone)]
pub struct Binding<'a> {
    /// The name of the binding
    pub name: &'a str,

    /// The kind of binding
    pub kind: BindingKind,
    /// The span of the name in source
    pub name_span: Span,
    /// The span of the entire declaration
    pub decl_span: Span,
    /// The scope this binding belongs to
    pub scope: ScopeId,
    /// Whether this binding has been referenced (used)
    pub is_used: bool,
    /// Whether this binding has been assigned a value
    pub is_initialized: bool,
    /// Minimum number of parameters/required args (if this is a function)
    pub min_parameter_count: Option<usize>,
    /// Maximum number of parameters/total args (if this is a function)
    pub parameter_count: Option<usize>,
    /// Whether this is a constructor function
    pub is_constructor: bool,
    /// Whether this is a variadic function (uses argument_count or argument)
    pub is_variadic: bool,
}



impl<'a> Binding<'a> {
    #[inline]
    pub fn new(
        name: &'a str,
        kind: BindingKind,
        name_span: Span,
        decl_span: Span,
        scope: ScopeId,
    ) -> Self {

        let is_initialized = !matches!(kind, BindingKind::UninitializedVariable);
        Self {
            name,
            kind,
            name_span,
            decl_span,
            scope,
            is_used: false,
            is_initialized,
            min_parameter_count: None,
            parameter_count: None,
            is_constructor: false,
            is_variadic: false,
        }
    }


    #[inline]
    pub fn with_params(mut self, min_count: usize, max_count: usize) -> Self {
        self.min_parameter_count = Some(min_count);
        self.parameter_count = Some(max_count);
        self
    }
}


/// The kind of scope
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Global/file scope
    Global,
    /// Function body
    Function,
    /// Block (if, for, while, etc.)
    Block,
}

/// Inline storage for up to 8 bindings per scope (covers most cases)
type ScopeBindings<'a> = SmallVec<[(&'a str, BindingId); 8]>;

/// A scope tracks bindings within a lexical scope
#[derive(Debug)]
pub struct Scope<'a> {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    /// If this is a function scope, the binding ID of the function
    pub function_binding: Option<BindingId>,
    /// Bindings in this scope - SmallVec for inline storage
    bindings: ScopeBindings<'a>,
}


impl<'a> Scope<'a> {
    #[inline]
    pub fn new(kind: ScopeKind, parent: Option<ScopeId>) -> Self {
        Self {
            kind,
            parent,
            function_binding: None,
            bindings: SmallVec::new(),
        }
    }
    
    #[inline]
    pub fn get(&self, name: &str) -> Option<BindingId> {
        // Linear search is fine for small scopes (typically < 10 bindings)
        self.bindings.iter()
            .find(|(n, _)| *n == name)
            .map(|(_, id)| *id)
    }
    
    #[inline]
    pub fn add(&mut self, name: &'a str, id: BindingId) {
        self.bindings.push((name, id));
    }
    
    pub fn binding_ids(&self) -> impl Iterator<Item = BindingId> + '_ {
        self.bindings.iter().map(|(_, id)| *id)
    }
}


/// The semantic model collects bindings and references during traversal
#[derive(Debug)]
pub struct SemanticModel<'a> {
    /// All bindings in the program (pre-allocated with capacity)
    bindings: Vec<Binding<'a>>,
    /// All scopes in the program
    scopes: Vec<Scope<'a>>,
    /// Current scope being processed
    current_scope: ScopeId,
    /// Name lookup cache for frequently accessed names
    name_cache: FxHashMap<&'a str, BindingId>,
}


impl<'a> Default for SemanticModel<'a> {
    fn default() -> Self {
        Self::new()
    }
}


impl<'a> SemanticModel<'a> {
    /// Create a new semantic model with pre-allocated capacity
    pub fn new() -> Self {
        Self::with_capacity(64, 16)
    }
    
    /// Create with specific capacity estimates
    pub fn with_capacity(bindings_capacity: usize, scopes_capacity: usize) -> Self {
        // Start with a global scope
        let global_scope = Scope::new(ScopeKind::Global, None);
        let mut scopes = Vec::with_capacity(scopes_capacity);
        scopes.push(global_scope);
        
        Self {
            bindings: Vec::with_capacity(bindings_capacity),
            scopes,
            current_scope: ScopeId::global(),
            name_cache: FxHashMap::with_capacity_and_hasher(bindings_capacity, Default::default()),
        }
    }

    
    /// Push a new scope and return its ID
    #[inline]
    pub fn push_scope(&mut self, kind: ScopeKind) -> ScopeId {
        let id = ScopeId::new(self.scopes.len() as u32);
        self.scopes.push(Scope::new(kind, Some(self.current_scope)));
        self.current_scope = id;
        id
    }
    
    /// Pop the current scope, returning to parent
    #[inline]
    pub fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current_scope.index()].parent {
            self.current_scope = parent;
        }
    }

    /// Set the function binding for a scope
    #[inline]
    pub fn set_scope_function_binding(&mut self, scope: ScopeId, binding: BindingId) {
        self.scopes[scope.index()].function_binding = Some(binding);
    }

    /// Get the binding ID of the nearest enclosing function
    pub fn current_function_binding(&self) -> Option<BindingId> {
        let mut curr = self.current_scope;
        loop {
            let scope = &self.scopes[curr.index()];
            if let Some(id) = scope.function_binding {
                return Some(id);
            }
            if let Some(parent) = scope.parent {
                curr = parent;
            } else {
                return None;
            }
        }
    }
    
    /// Add a binding to the current scope
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub fn add_binding(
        &mut self,
        name: &'a str,
        kind: BindingKind,
        name_span: Span,
        decl_span: Span,
        min_parameter_count: Option<usize>,
        max_parameter_count: Option<usize>,
        is_constructor: bool,
    ) -> BindingId {
        let id = BindingId::new(self.bindings.len() as u32);
        let mut binding = Binding::new(name, kind, name_span, decl_span, self.current_scope);
        binding.min_parameter_count = min_parameter_count;
        binding.parameter_count = max_parameter_count;
        binding.is_constructor = is_constructor;
        self.bindings.push(binding);


        self.scopes[self.current_scope.index()].add(name, id);
        
        // Cache for fast lookup
        self.name_cache.insert(name, id);
        id
    }

    
    /// Mark a binding as initialized (for variables that get assigned later)
    #[inline]
    pub fn mark_initialized(&mut self, id: BindingId) {
        if let Some(binding) = self.bindings.get_mut(id.index()) {
            binding.is_initialized = true;
        }
    }

    /// Mark a binding as variadic
    #[inline]
    pub fn mark_variadic(&mut self, id: BindingId) {
        if let Some(binding) = self.bindings.get_mut(id.index()) {
            binding.is_variadic = true;
        }
    }
    
    /// Record a reference to a name (usage)
    #[inline]
    pub fn add_reference(&mut self, name: &'a str) -> Option<BindingId> {
        // Look up the binding in current scope and ancestors
        if let Some(id) = self.resolve_name(name) {
            self.bindings[id.index()].is_used = true;
            Some(id)
        } else {
            None // Name not found (might be global/builtin)
        }
    }

    
    /// Resolve a name to a binding ID by searching up the scope chain
    #[inline]
    pub fn resolve_name(&self, name: &str) -> Option<BindingId> {
        // First check name cache (covers most lookups)
        if let Some(&id) = self.name_cache.get(name) {
            // Verify it's visible in current scope chain
            let binding_scope = self.bindings[id.index()].scope;
            if self.is_scope_visible(binding_scope) {
                return Some(id);
            }
        }
        
        // Fall back to scope chain lookup
        let mut scope_id = Some(self.current_scope);
        while let Some(id) = scope_id {
            let scope = &self.scopes[id.index()];
            if let Some(binding_id) = scope.get(name) {
                return Some(binding_id);
            }
            scope_id = scope.parent;
        }
        None
    }

    /// Check if a scope is visible from current scope (in ancestor chain)
    #[inline]
    fn is_scope_visible(&self, target: ScopeId) -> bool {
        if target == ScopeId::global() {
            return true;
        }
        
        let mut scope_id = Some(self.current_scope);
        while let Some(id) = scope_id {
            if id == target {
                return true;
            }
            scope_id = self.scopes[id.index()].parent;
        }
        false
    }



    /// Get a binding by ID
    #[inline]
    pub fn get_binding(&self, id: BindingId) -> Option<&Binding<'a>> {
        self.bindings.get(id.index())
    }
    
    /// Get all bindings
    #[inline]
    pub fn bindings(&self) -> &[Binding<'a>] {
        &self.bindings
    }
    
    /// Get all unused bindings (for GML003)
    pub fn unused_bindings(&self) -> impl Iterator<Item = &Binding<'a>> {
        self.bindings.iter().filter(|b| {
            !b.is_used 
            && !b.name.starts_with('_')
            && !matches!(b.kind, BindingKind::Function | BindingKind::CatchVar)
        })
    }
    
    /// Get all uninitialized bindings that were used (for GML001)
    pub fn uninitialized_usages(&self) -> impl Iterator<Item = &Binding<'a>> {
        self.bindings.iter().filter(|b| {
            !b.is_initialized && b.is_used
        })
    }

    
    /// Get current scope ID
    #[inline]
    pub fn current_scope(&self) -> ScopeId {
        self.current_scope
    }
}
