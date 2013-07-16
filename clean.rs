use its = syntax::parse::token::ident_to_str;

use syntax;
use syntax::ast;

use super::doctree;
use std::local_data::*;

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

pub struct Method {
    ident: ~str,
    attrs: ~[Attribute],
    generics: Generics,
    //explicit_self: ExplicitSelf,
    id: ast::node_id,
    vis: Visibility
}

pub struct TyMethod {
    ident: ~str,
    attrs: ~[Attribute],
    generics: Generics,
    id: ast::node_id,

}

pub enum TraitMethod {
    Required(TyMethod),
    Provided(Method)
}

pub struct Function {
    decl: FnDecl,
    //body: Block,
    id: ast::node_id,
    attrs: ~[Attribute]
}

pub struct FnDecl {
    inputs: ~[Argument],
    output: @Type,
    cf: RetStyle,
    attrs: ~[Attribute]
}

pub struct Argument {
    mutable: bool,
    ty: @Type,
    //TODO pat
    id: ast::node_id
}

pub enum RetStyle {
    NoReturn,
    Return
}

pub struct Trait {
    name: ~str,
    methods: ~[Method], //should be TraitMethod
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
    Unique(~Type),
    Managed(~Type),
    // region, raw, other boxes, mutable
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
            Unit => Unit,
            Unique(ref __self_0) => Unique(__self_0.clone()),
            Managed(ref __self_0) => Managed(__self_0.clone())
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

pub struct Enum {
    variants: ~[Variant],
    generics: Generics,
    attrs: ~[Attribute]
}

pub struct Variant {
    name: ~str,
    attrs: ~[Attribute],
    //kind: ast::variant_kind,
    id: ast::node_id,
    visibility: Visibility
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
    pub fn clean(&self) -> Attribute {
        match *self {
            ast::meta_word(s) => Word(s.to_owned()),
            ast::meta_list(ref s, ref l) => List(s.to_owned(), l.iter()
                                         .transform(|x| x.node.clean()).collect()),
            ast::meta_name_value(s, ref v) => NameValue(s.to_owned(), lit_to_str(v))
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

fn resolve_type(t: &Type) -> Type {
    use syntax::ast::*;

    let id = match t {
        &Unresolved(id) => id,
        _ => return (*t).clone()
    };

    let dm = unsafe { local_data_get(super::ctxtkey).unwrap() }.cmap.def_map;
    debug!("searching for %? in defmap", id);
    let d = match dm.find(&id) {
        Some(k) => k,
        None => {
            let ctxt = unsafe { local_data_get(super::ctxtkey).unwrap() };
            debug!("could not find %? in defmap (`%s`)", id,
                   syntax::ast_map::node_id_to_str(ctxt.amap, id, ctxt.sess.intr()));
            fail!("Unexpected failure: unresolved id not in defmap (this is a bug!)");
        }
    };

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
    pub fn clean(&self) -> Type {
        use syntax::ast::*;
        debug!("cleaning type `%?`", self);
        let codemap = unsafe { local_data_get(super::ctxtkey).unwrap() }.sess.codemap;
        debug!("span corresponds to `%s`", codemap.span_to_str(self.span));
        let mut t = match self.node {
            ty_nil => Unit,
            ty_ptr(m) | ty_rptr(_, m) => resolve_type(&m.ty.clean()),
            ty_box(m) => Managed(~resolve_type(&m.ty.clean())),
            ty_uniq(m) => Unique(~resolve_type(&m.ty.clean())),
            ty_vec(m) | ty_fixed_length_vec(m, _) =>
                Vector(~resolve_type(&m.ty.clean())),
            ty_tup(ref tys) => Tuple(tys.iter().transform(|x|
                                                          resolve_type(&x.clean())).collect()),
            ty_path(_, _, id) => Unresolved(id),
            _ => fail!("Unimplemented type (this is a bug"),
        };
        resolve_type(&t)
    }
}

impl Clean<Enum> for doctree::Enum {
    pub fn clean(&self) -> Enum {
        Enum {
            variants: self.variants.iter().transform(|x| x.clean()).collect(),
            generics: self.generics.clean(),
            attrs: self.attrs.iter().transform(|x| x.clean()).collect()
        }
    }
}

impl Clean<Variant> for doctree::Variant {
    pub fn clean(&self) -> Variant {
        Variant {
            name: its(&self.name).to_owned(),
            attrs: self.attrs.iter().transform(|x| x.clean()).collect(),
            //kind: self.kind,
            id: self.id,
            visibility: self.visibility
        }
    }
}

impl Clean<Function> for doctree::Function {
    pub fn clean(&self) -> Function {
        Function {
            decl: self.decl.clean(),
            id: self.id,
            attrs: ~[]
        }
    }
}

impl Clean<FnDecl> for ast::fn_decl {
    pub fn clean(&self) -> FnDecl {
        FnDecl {
            inputs: self.inputs.iter().transform(|x| x.clean()).collect(),
            output: @(self.output.clean()),
            cf: self.cf.clean(),
            attrs: ~[]
        }
    }
}

impl Clean<Argument> for ast::arg {
    pub fn clean(&self) -> Argument {
        Argument {
            mutable: self.is_mutbl,
            ty: @(self.ty.clean()),
            id: self.id
        }
    }
}

impl Clean<RetStyle> for ast::ret_style {
    pub fn clean(&self) -> RetStyle {
        match *self {
            ast::return_val => Return,
            ast::noreturn => NoReturn
        }
    }
}
