use super::types::Type;
use std::cell::RefCell;
use rustc_hash::FxHashMap;

pub trait Db {
    fn intern_type(&self, ty: Type) -> TypeId;
    fn lookup_type(&self, id: TypeId) -> Type;
    
    fn define_struct(&self) -> TypeId;
    fn add_struct_field(&self, struct_id: TypeId, name: String, ty: Type);
    fn lookup_struct_field(&self, struct_id: TypeId, name: &str) -> Option<Type>;
    fn get_struct_fields(&self, struct_id: TypeId) -> Vec<(String, Type)>;

    fn define_function(&self) -> TypeId;
    fn set_return_type(&self, fn_id: TypeId, ty: Type);
    fn lookup_return_type(&self, fn_id: TypeId) -> Type;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

#[derive(Default)]
pub struct Database {
    types: RefCell<FxHashMap<TypeId, Type>>,
    rev_types: RefCell<FxHashMap<Type, TypeId>>,
    struct_fields: RefCell<FxHashMap<TypeId, FxHashMap<String, Type>>>,
    function_returns: RefCell<FxHashMap<TypeId, Type>>,
    next_id: RefCell<u32>,
}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Db for Database {
    fn intern_type(&self, ty: Type) -> TypeId {
        if let Some(&id) = self.rev_types.borrow().get(&ty) {
            return id;
        }
        
        let id = TypeId(*self.next_id.borrow());
        *self.next_id.borrow_mut() += 1;
        
        self.types.borrow_mut().insert(id, ty.clone());
        self.rev_types.borrow_mut().insert(ty, id);
        
        id
    }

    fn lookup_type(&self, id: TypeId) -> Type {
        self.types.borrow().get(&id).unwrap().clone()
    }

    fn define_struct(&self) -> TypeId {
        let id = TypeId(*self.next_id.borrow());
        *self.next_id.borrow_mut() += 1;
        
        self.types.borrow_mut().insert(id, Type::Struct(id));
        self.struct_fields.borrow_mut().insert(id, FxHashMap::default());
        
        id
    }

    fn add_struct_field(&self, struct_id: TypeId, name: String, ty: Type) {
        if let Some(map) = self.struct_fields.borrow_mut().get_mut(&struct_id) {
            map.insert(name, ty);
        }
    }

    fn lookup_struct_field(&self, struct_id: TypeId, name: &str) -> Option<Type> {
        self.struct_fields.borrow().get(&struct_id)?.get(name).cloned()
    }

    fn get_struct_fields(&self, struct_id: TypeId) -> Vec<(String, Type)> {
        if let Some(map) = self.struct_fields.borrow().get(&struct_id) {
            map.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
        } else {
            vec![]
        }
    }

    fn define_function(&self) -> TypeId {
        let id = TypeId(*self.next_id.borrow());
        *self.next_id.borrow_mut() += 1;
        
        self.types.borrow_mut().insert(id, Type::Function(id));
        self.function_returns.borrow_mut().insert(id, Type::Any);
        
        id
    }

    fn set_return_type(&self, fn_id: TypeId, ty: Type) {
        self.function_returns.borrow_mut().insert(fn_id, ty);
    }

    fn lookup_return_type(&self, fn_id: TypeId) -> Type {
        self.function_returns.borrow().get(&fn_id).cloned().unwrap_or(Type::Any)
    }
}

