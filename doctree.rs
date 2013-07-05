//! The simplified AST used by rustdoc

use syntax::ast;
use syntax::ast::{ident, node_id};

#[deriving(ToStr)]
pub enum StructType {
    /// A normal struct
    Plain,
    /// A tuple struct
    Tuple,
    /// A newtype struct (tuple struct with one element)
    Newtype,
    /// A unit struct
    Unit
}

pub enum TypeBound {
    RegionBound,
    TraitBound(ast::trait_ref)
}

pub struct StructField {
    id: node_id,
    type_: @ast::Ty,
    /// Name is optional for tuple structs
    name: Option<ident>,
    attrs: ~[ast::attribute],
    visibility: Option<ast::visibility>
}

pub struct Struct {
    node: node_id,
    struct_type: StructType,
    name: ident,
    generics: ast::Generics,
    attrs: ~[ast::attribute],
    fields: ~[StructField]
}
