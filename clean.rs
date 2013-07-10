use its = syntax::parse::token::ident_to_str;

use syntax::ast;

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

/// A representation of a Type suitable for hyperlinking purposes. Ideally one can get the original
/// type out of the AST/ty::ctxt given one of these, if more information is needed. Most importantly
/// it does not preserve mutability or boxes.
pub enum Type {
    /// Most types start out as "Unresolved". It serves as an intermediate stage between cleaning
    /// and type resolution.
    Unresolved(ast::node_id),
    /// structs/enums/traits (anything that'd be an ast::ty_path)
    Resolved(ast::node_id),
    Primitive(ast::prim_ty),
    Tuple(~[Type]),
    Vector(~Type),
    String,
    Bool,
    /// aka ty_nil
    Unit,
}

impl Clone for Type {
    pub fn clone(&self) -> Type {
        match *self {
            Unresolved(ref __self_0) => Unresolved(__self_0.clone()),
            Resolved(ref __self_0) => Resolved(__self_0.clone()),
            Primitive(ref __self_0) => Primitive(*__self_0.clone()),
            Tuple(ref __self_0) => Tuple(__self_0.clone()),
            Vector(ref __self_0) => Vector(__self_0.clone()),
            String => String,
            Bool => Bool,
            Unit => Unit
        }
    }
}

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
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> T;
}

impl Clean<Struct> for doctree::Struct {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> Struct {
        Struct {
            name: its(&self.name).to_owned(),
            node: self.node,
            struct_type: self.struct_type,
            attrs: self.attrs.iter().transform(|x| x.clean(tcx)).collect(),
            generics: self.generics.clean(tcx),
            fields: self.fields.iter().transform(|x| x.clean(tcx)).collect()
        }
    }
}

impl Clean<Generics> for ast::Generics {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> Generics {
        Generics {
            lifetimes: self.lifetimes.iter().transform(|x| x.clean(tcx)).collect(),
            type_params: self.ty_params.iter().transform(|x| x.clean(tcx)).collect()
        }
    }
}

impl Clean<Attribute> for ast::attribute {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> Attribute {
        self.node.value.node.clean(tcx)
    }
}

fn lit_to_str(lit: &ast::lit) -> ~str {
    match lit.node {
        ast::lit_str(st) => st.to_owned(),
        ast::lit_int(ch, ast::ty_char) => ~"'" + ch.to_str() + "'",
        ast::lit_int(i, _t) => i.to_str(),
        ast::lit_uint(u, _t) => u.to_str(),
        ast::lit_int_unsuffixed(i) => i.to_str(),
        ast::lit_float(f, _t) => f.to_str(),
        ast::lit_float_unsuffixed(f) => f.to_str(),
        ast::lit_bool(b) => b.to_str(),
        ast::lit_nil => ~"",
    }
}

impl Clean<Attribute> for ast::meta_item_ {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> Attribute {
        match *self {
            ast::meta_word(s) => Word(s.to_owned()),
            ast::meta_list(ref s, ref l) => List(s.to_owned(), l.iter()
                                         .transform(|x| x.node.clean(tcx)).collect()),
            ast::meta_name_value(s, ref v) => NameValue(s.to_owned(), lit_to_str(v))
        }
    }
}

impl Clean<StructField> for doctree::StructField {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> StructField {
        StructField {
            name: if self.name.is_some() { its(&self.name.unwrap()).to_owned() } else { ~"" },
            type_: self.type_.clean(tcx),
            attrs: self.attrs.iter().transform(|x| x.clean(tcx)).collect(),
            visibility: self.visibility
        }
    }
}

impl Clean<Lifetime> for ast::Lifetime {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> Lifetime {
        Lifetime(its(&self.ident).to_owned())
    }
}

impl Clean<TyParamBound> for ast::TyParamBound {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> TyParamBound {
        match *self {
            ast::RegionTyParamBound => RegionBound,
            ast::TraitTyParamBound(_t) => TraitBound(Trait::new())
        }
    }
}

impl Clean<TyParam> for ast::TyParam {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> TyParam {
        TyParam {
            name: its(&self.ident).to_owned(),
            bounds: self.bounds.iter().transform(|x| x.clean(tcx)).collect()
        }
    }
}

fn resolve_type(dm: &super::rustc::middle::resolve::DefMap, t: &Type) -> Type {
    use syntax::ast::*;

    let id = match t {
        &Unresolved(id) => id,
        _ => return (*t).clone()
    };
    let d = dm.find(&id).expect("unresolved type maps to no def (this is a bug)");

    Resolved(match *d {
        def_fn(i, _) => i.node,
        def_self(i, _) => i,
        def_self_ty(i) => i,
        def_ty(i) => i.node,
        def_trait(i) => {
            debug!("saw def_trait in def_to_id");
            i.node
        },
        def_prim_ty(p) => match p {
            ty_str => return String,
            ty_bool => return Bool,
            _ => return Primitive(p)
        },
        def_ty_param(i, _) => i.node,
        def_struct(i) => i.node,
        def_typaram_binder(i) => i,
        _ => fail!("resolved type maps to a weird def"),
    })
}

impl Clean<Type> for ast::Ty {
    pub fn clean(&self, tcx: super::rustc::middle::ty::ctxt) -> Type {
        use syntax::ast::*;
        let mut t = match self.node {
            ty_nil => Unit,
            ty_box(m) | ty_uniq(m) | ty_ptr(m) | ty_rptr(_, m) => Unresolved(m.ty.id),
            ty_vec(m) | ty_fixed_length_vec(m, _) =>
                Vector(~resolve_type(&tcx.def_map, &m.ty.clean(tcx))),
            ty_tup(ref tys) => Tuple(tys.iter().transform(|x|
                                                          resolve_type(&tcx.def_map,
                                                          &x.clean(tcx))).collect()),
            ty_path(_, _, id) => Unresolved(id),
            _ => fail!("Unimplemented type (this is a bug"),
        };
        resolve_type(&tcx.def_map, &t)
    }
}
