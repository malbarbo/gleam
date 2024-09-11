use ecow::EcoString;
use std::fmt::Debug;
use std::hash::Hash;
use std::{collections::HashMap, marker::PhantomData};

/// A unique identifier parameterized by a type.
pub struct Id<T> {
    /// The index of the identifier.
    index: u32,

    _marker: PhantomData<T>,
}

impl<T> Id<T> {
    pub fn new(index: u32) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }

    pub fn id(&self) -> u32 {
        self.index
    }
}

// manual implementation of following traits
impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for Id<T> {}

impl<T> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _marker: PhantomData,
        }
    }
}

impl<T> Copy for Id<T> {}

impl<T> Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id({})", self.index)
    }
}

impl<T> Default for Id<T> {
    fn default() -> Self {
        Self {
            index: 0,
            _marker: PhantomData,
        }
    }
}

/// Unique identifier for types.
pub type TypeId = Id<Type>;

/// Unique identifier for functions.
pub type FunctionId = Id<Function>;

/// Sum type identifier.
pub type SumId = Id<Sum>;

/// Product type identifier.
pub type ProductId = Id<Product>;

/// Constant identifier.
pub type ConstantId = Id<Constant>;

// bring over WasmType later.
pub type TypeDefinition = super::WasmType;

/// Represents a type in the type table.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    /// The unique identifier of the type.
    pub id: TypeId,

    /// The name of the type, for debugging purposes.
    pub name: EcoString,

    /// The definition of the type.
    pub definition: TypeDefinition,
}

/// Represents a function in the function table.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    /// The unique identifier of the function.
    pub id: FunctionId,

    /// The name of the function, for debugging purposes.
    pub name: EcoString,

    /// The function's type signature.
    pub signature: TypeId,

    /// Arity.
    pub arity: u32,
}

/// Represents a sum type in the type table.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Sum {
    /// The unique identifier of the sum type.
    pub id: SumId,

    /// The name of the sum type, for debugging purposes.
    pub name: EcoString,

    /// The type of the sum type. Used for runtime type checking.
    pub type_: TypeId,

    /// The variants of the sum type.
    pub variants: Vec<ProductId>,
}

/// Represents a product type in the type table.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Product {
    /// The unique identifier of the product type.
    pub id: ProductId,

    /// The name of the product type, for debugging purposes.
    pub name: EcoString,

    /// The type of the product type. Used for runtime type checking.
    pub type_: TypeId,

    /// The parent sum type of the product type.
    pub parent: SumId,

    /// Whether the product type is a simple product (has no fields, has a singleton instance),
    /// or a composite product (has fields, has a constructor).
    pub kind: ProductKind,
}

/// The kind of a product type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProductKind {
    /// A simple product type. This product type has no fields and has a singleton instance stored as a constant.
    Simple {
        /// The constant identifier of the singleton instance.
        instance: ConstantId,
    },

    /// A composite product type. This product type has fields and a constructor function.
    Composite {
        /// The constructor function of the product type.
        constructor: FunctionId,
    },
}

/// Represents a constant in the constant table.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constant {
    /// The unique identifier of the constant.
    pub id: ConstantId,

    /// The constant's type.
    pub type_: TypeId,
}

#[derive(Debug, Clone)]
pub struct Store<T: Clone + Debug> {
    items: HashMap<Id<T>, T>,
    next: u32,
}

impl<T: Clone + Debug> Store<T> {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
            next: 0,
        }
    }

    pub fn new_id(&mut self) -> Id<T> {
        let id = Id::new(self.next);
        self.next += 1;
        id
    }

    pub fn insert(&mut self, id: Id<T>, item: T) {
        _ = self.items.insert(id, item);
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.items.get(&id)
    }

    #[deprecated]
    pub fn get_from_id(&self, id: u32) -> Option<&T> {
        self.items.get(&Id::new(id))
    }

    pub fn as_list(&self) -> Vec<T> {
        let mut lst = vec![];
        for i in 0..self.next {
            let id = Id::new(i);
            if let Some(item) = self.items.get(&id) {
                lst.push(item.clone());
            }
        }
        lst
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub types: Store<Type>,
    pub functions: Store<Function>,
    pub sums: Store<Sum>,
    pub products: Store<Product>,
    pub constants: Store<Constant>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            types: Store::new(),
            functions: Store::new(),
            sums: Store::new(),
            products: Store::new(),
            constants: Store::new(),
        }
    }
}
