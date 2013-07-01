//! The simplified AST used by rustdoc

use syntax::ast;
use syntax::ast::{ident, node_id};

pub type Attribute = ast::attribute;

pub enum StructureType {
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

pub struct StructureField {
    id: node_id,
    type_: @ast::Ty,
    attrs: ~[Attribute],
    /// Name is optional for tuple structs
    name: Option<ident>,
    visibility: Option<ast::visibility>
}

pub struct Structure {
    node: node_id,
    struct_type: StructureType,
    name: ident,
    type_params: ~[ast::TyParam],
    lifetimes: ~[ast::Lifetime],
    fields: ~[StructureField]
}
