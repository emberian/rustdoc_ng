//! This module is used to store stuff from Rust's AST in a more convenient
//! manner (and with prettier names) before cleaning.

use syntax;
use syntax::codemap::span;
use syntax::ast;
use syntax::ast::{ident, NodeId};

pub struct Module {
    name: Option<ident>,
    attrs: ~[ast::Attribute],
    where: span,
    structs: ~[Struct],
    enums: ~[Enum],
    fns: ~[Function],
    mods: ~[Module],
    typedefs: ~[Typedef],
    statics: ~[Static],
    traits: ~[Trait],
    impls: ~[Impl],
    view_items: ~[ast::view_item],
}

impl Module {
    pub fn new(name: Option<ident>) -> Module {
        Module {
            name       : name,
            where: syntax::codemap::dummy_sp(),
            attrs      : ~[],
            structs    : ~[],
            enums      : ~[],
            fns        : ~[],
            mods       : ~[],
            typedefs   : ~[],
            statics    : ~[],
            traits     : ~[],
            impls      : ~[],
            view_items : ~[],
        }
    }
}

#[deriving(ToStr, Clone, Encodable, Decodable)]
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

pub struct Struct {
    id: NodeId,
    struct_type: StructType,
    name: ident,
    generics: ast::Generics,
    attrs: ~[ast::Attribute],
    fields: ~[@ast::struct_field],
    where: span,
}

pub struct Enum {
    variants: ~[Variant],
    generics: ast::Generics,
    attrs: ~[ast::Attribute],
    id: NodeId,
    where: span,
    name: ident,
}

pub struct Variant {
    name: ident,
    attrs: ~[ast::Attribute],
    kind: ast::variant_kind,
    visibility: ast::visibility,
    where: span,
}

pub struct Function {
    decl: ast::fn_decl,
    attrs: ~[ast::Attribute],
    id: NodeId,
    name: ident,
    visibility: ast::visibility,
    where: span,
    generics: ast::Generics,
}

pub struct Typedef {
    ty: ast::Ty,
    gen: ast::Generics,
    name: ast::ident,
    id: ast::NodeId,
    attrs: ~[ast::Attribute],
    where: span,
}

pub struct Static {
    type_: ast::Ty,
    mutability: ast::mutability,
    expr: @ast::expr,
    name: ast::ident,
    attrs: ~[ast::Attribute],
    where: span,
}

pub struct Trait {
    name: ast::ident,
    methods: ~[ast::trait_method], //should be TraitMethod
    generics: ast::Generics,
    parents: ~[ast::trait_ref],
    attrs: ~[ast::Attribute],
    id: ast::NodeId,
    where: span,
}

pub struct Impl {
    generics: ast::Generics,
    trait_: Option<ast::trait_ref>,
    for_: ast::Ty,
    methods: ~[@ast::method],
    attrs: ~[ast::Attribute],
    where: span,
}

pub fn struct_type_from_def(sd: &ast::struct_def) -> StructType {
    if sd.ctor_id.is_some() {
        // We are in a tuple-struct
        match sd.fields.len() {
            0 => Unit,
            1 => Newtype,
            _ => Tuple
        }
    } else {
        Plain
    }
}
