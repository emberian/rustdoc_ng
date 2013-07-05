use its = syntax::parse::token::ident_to_str;

use syntax::ast;
use syntax::print::pprust::lit_to_str;

use super::doctree;

pub enum Attribute {
    Word(~str),
    List(~str, ~[Attribute]),
    NameValue(~str, ~str)
}

pub struct TyParam {
    name: ~str,
    bounds: ~[TyParamBound]
}

pub enum TyParamBound {
    RegionBound,
    TraitBound(Trait)
}

pub struct Lifetime(~str);

// maybe use a Generic enum and use ~[Generic]?
pub struct Generics {
    lifetimes: ~[Lifetime],
    type_params: ~[TyParam]
}

impl Generics {
    pub fn new() -> Generics {
        Generics {
            lifetimes: ~[],
            type_params: ~[]
        }
    }
}

pub struct Method;

pub struct Trait {
    name: ~str,
    methods: ~[Method],
    lifetimes: ~[Lifetime],
    generics: Generics
}

impl Trait {
    pub fn new() -> Trait {
        Trait {
            name: ~"",
            methods: ~[],
            lifetimes: ~[],
            generics: Generics::new()
        }
    }
}

pub struct Type(uint);

pub struct StructField {
    name: ~str,
    type_: Type,
    attrs: ~[Attribute],
    visibility: Option<Visibility>
}

pub type Visibility = ast::visibility;

pub struct Struct {
    name: ~str,
    node: ast::node_id,
    struct_type: doctree::StructType,
    attrs: ~[Attribute],
    generics: Generics,
    fields: ~[StructField]
}

pub trait Clean<T> {
    pub fn clean(&self) -> T;
}

impl Clean<Struct> for doctree::Struct {
    pub fn clean(&self) -> Struct {
        Struct {
            name: its(&self.name).to_owned(),
            node: self.node,
            struct_type: self.struct_type,
            attrs: self.attrs.iter().transform(|x| x.clean()).collect(),
            generics: self.generics.clean(),
            fields: self.fields.iter().transform(|x| x.clean()).collect()
        }
    }
}

impl Clean<Generics> for ast::Generics {
    pub fn clean(&self) -> Generics {
        Generics {
            lifetimes: self.lifetimes.iter().transform(|x| x.clean()).collect(),
            type_params: self.ty_params.iter().transform(|x| x.clean()).collect()
        }
    }
}

impl Clean<Attribute> for ast::attribute {
    pub fn clean(&self) -> Attribute {
        self.node.value.node.clean()
    }
}

impl Clean<Attribute> for ast::meta_item_ {
    pub fn clean(&self) -> Attribute {
        match *self {
            ast::meta_word(s) => Word(s.to_owned()),
            ast::meta_list(ref s, ref l) => List(s.to_owned(), l.iter()
                                         .transform(|x| x.node.clean()).collect()),
            ast::meta_name_value(s, v) => NameValue(s.to_owned(), lit_to_str(@v))
        }
    }
}

impl Clean<StructField> for doctree::StructField {
    pub fn clean(&self) -> StructField {
        StructField {
            name: if self.name.is_some() { its(&self.name.unwrap()).to_owned() } else { ~"" },
            type_: self.type_.clean(),
            attrs: self.attrs.iter().transform(|x| x.clean()).collect(),
            visibility: self.visibility
        }
    }
}

impl Clean<Lifetime> for ast::Lifetime {
    pub fn clean(&self) -> Lifetime {
        Lifetime(its(&self.ident).to_owned())
    }
}

impl Clean<TyParamBound> for ast::TyParamBound {
    pub fn clean(&self) -> TyParamBound {
        match *self {
            ast::RegionTyParamBound => RegionBound,
            ast::TraitTyParamBound(_t) => TraitBound(Trait::new())
        }
    }
}

impl Clean<TyParam> for ast::TyParam {
    pub fn clean(&self) -> TyParam {
        TyParam {
            name: its(&self.ident).to_owned(),
            bounds: self.bounds.iter().transform(|x| x.clean()).collect()
        }
    }
}

impl Clean<Type> for ast::Ty {
    pub fn clean(&self) -> Type {
        Type(self.id as uint)
    }
}
