//! The simplified AST used by rustdoc

use syntax::codemap::span;
use syntax::ast;
use syntax::ast::{ident, node_id};

pub struct Module {
    name: Option<ident>,
    attrs: ~[ast::Attribute],
    structs: ~[Struct],
    enums: ~[Enum],
    fns: ~[Function],
    mods: ~[Module]
}

impl Module {
    pub fn new(name: Option<ident>) -> Module {
        Module {
            name: name,
            attrs: ~[],
            structs: ~[],
            enums: ~[],
            fns: ~[],
            mods: ~[]
        }
    }
}

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
    type_: ast::Ty,
    /// Name is optional for tuple structs
    name: Option<ident>,
    attrs: ~[ast::Attribute],
    visibility: Option<ast::visibility>
}

impl StructField {
    pub fn new(sf: &ast::struct_field_) -> StructField {
        let (name, vis) = match sf.kind {
            ast::named_field(id, vis) => (Some(id), Some(vis)),
            _ => (None, None)
        };
        StructField {
            id: sf.id,
            type_: sf.ty.clone(),
            attrs: sf.attrs.clone(),
            name: name,
            visibility: vis,
        }
    }
}

pub struct Struct {
    id: node_id,
    struct_type: StructType,
    name: ident,
    generics: ast::Generics,
    attrs: ~[ast::Attribute],
    fields: ~[StructField],
    where: span,
}

pub struct Enum {
    variants: ~[Variant],
    generics: ast::Generics,
    attrs: ~[ast::Attribute],
    id: node_id,
    where: span,
    name: ident,
}

pub struct Variant {
    name: ident,
    attrs: ~[ast::Attribute],
    kind: ast::variant_kind,
    visibility: ast::visibility
}

pub struct Function {
    decl: ast::fn_decl,
    attrs: ~[ast::Attribute],
    id: node_id,
    name: ident,
    visibility: ast::visibility,
    where: span,
    generics: ast::Generics,
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
