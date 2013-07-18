use its = syntax::parse::token::ident_to_str;

use syntax;
use syntax::ast;

use doctree;
use visit;
use std::local_data;

#[deriving(Clone)]
pub enum Attribute {
    Word(~str),
    List(~str, ~[Attribute]),
    NameValue(~str, ~str)
}

pub struct TyParam {
    name: ~str,
    node: ast::node_id,
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
    name: ~str,
    visibility: Visibility,
    where: ~str,
    generics: Generics,
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
#[deriving(Clone)]
pub enum Type {
    /// Most types start out as "Unresolved". It serves as an intermediate stage between cleaning
    /// and type resolution.
    Unresolved(ast::node_id),
    /// structs/enums/traits (anything that'd be an ast::ty_path)
    Resolved(ast::node_id),
    /// For parameterized types, so the consumer of the JSON don't go looking
    /// for types which don't exist anywhere.
    Generic(ast::node_id),
    /// For references to self
    Self(ast::node_id),
    /// Primitives are just the fixed-size numeric types (plus int/uint/float), and char.
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

pub struct StructField {
    name: ~str,
    type_: Type,
    attrs: ~[Attribute],
    visibility: Option<Visibility>,
}

pub type Visibility = ast::visibility;

pub struct Struct {
    name: ~str,
    where: ~str,
    node: ast::node_id,
    struct_type: doctree::StructType,
    attrs: ~[Attribute],
    generics: Generics,
    fields: ~[StructField],
}

/// This is a more limited form of the standard Struct, different in that it
/// it lacks the things most items have (name, id, parameterization). Found
/// only as a variant in an enum.
pub struct VariantStruct {
    struct_type: doctree::StructType,
    fields: ~[StructField],
}

pub struct Enum {
    variants: ~[Variant],
    generics: Generics,
    attrs: ~[Attribute],
    name: ~str,
    node: ast::node_id,
    where: ~str,
}

pub struct Variant {
    name: ~str,
    attrs: ~[Attribute],
    kind: VariantKind,
    visibility: Visibility,
}

pub enum VariantKind {
    CLikeVariant,
    TupleVariant(~[Type]),
    StructVariant(VariantStruct),
}

pub struct Crate {
    structs: ~[Struct],
    enums: ~[Enum],
    fns: ~[Function],
}

pub trait Clean<T> {
    pub fn clean(&self) -> T;
}

impl Clean<Crate> for visit::RustdocVisitor {
    pub fn clean(&self) -> Crate {
        Crate {
            structs: self.structs.iter().transform(|x| x.clean()).collect(),
            enums: self.enums.iter().transform(|x| x.clean()).collect(),
            fns: self.fns.iter().transform(|x| x.clean()).collect()
        }
    }
}

impl Clean<Struct> for doctree::Struct {
    pub fn clean(&self) -> Struct {
        Struct {
            name: its(&self.name).to_owned(),
            node: self.id,
            struct_type: self.struct_type,
            attrs: collapse_docs(self.attrs.iter().transform(|x| x.clean()).collect()),
            generics: self.generics.clean(),
            fields: self.fields.iter().transform(|x| x.clean()).collect(),
            where: self.where.clean(),
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

fn remove_comment_tags(s: &str) -> ~str {
    match s.slice(0,3) {
        &"///" => return s.slice(3, s.len()).trim().to_owned(),
        &"/**" | &"/*!" => return s.slice(3, s.len() - 2).trim().to_owned(),
        _ => return s.trim().to_owned()
    }
}

fn collapse_docs(attrs: ~[Attribute]) -> ~[Attribute] {
    let mut docstr = ~"";
    for attrs.iter().advance |at| {
        match *at {
            NameValue(~"doc", ref s) => docstr.push_str(fmt!("%s ", s.clone())),
            _ => ()
        }
    }
    let mut a = attrs.iter().filter(|&a| match a {
        &NameValue(~"doc", _) => false,
        _ => true
    }).transform(|x| x.clone()).collect::<~[Attribute]>();
    a.push(NameValue(~"doc", docstr.trim().to_owned()));
    a
}

impl Clean<Attribute> for ast::meta_item_ {
    pub fn clean(&self) -> Attribute {
        match *self {
            ast::meta_word(s) => Word(remove_comment_tags(s)),
            ast::meta_list(ref s, ref l) => List(s.to_owned(), l.iter()
                                         .transform(|x| x.node.clean()).collect()),
            ast::meta_name_value(s, ref v) => NameValue(remove_comment_tags(s),
                                         remove_comment_tags(lit_to_str(v)))
        }
    }
}

impl Clean<StructField> for doctree::StructField {
    pub fn clean(&self) -> StructField {
        StructField {
            name: if self.name.is_some() { its(&self.name.unwrap()).to_owned() } else { ~"" },
            type_: self.type_.clean(),
            attrs: collapse_docs(self.attrs.iter().transform(|x| x.clean()).collect()),
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
            ast::TraitTyParamBound(_) => TraitBound(Trait::new())
        }
    }
}

impl Clean<TyParam> for ast::TyParam {
    pub fn clean(&self) -> TyParam {
        TyParam {
            name: its(&self.ident).to_owned(),
            node: self.id,
            bounds: self.bounds.iter().transform(|x| x.clean()).collect()
        }
    }
}

/// Given a Type, resolve it using the def_map
fn resolve_type(t: &Type) -> Type {
    use syntax::ast::*;

    let id = match t {
        &Unresolved(id) => id,
        _ => return (*t).clone()
    };

    let dm = local_data::get(super::ctxtkey, |x| *x.unwrap()).tycx.def_map;
    debug!("searching for %? in defmap", id);
    let d = match dm.find(&id) {
        Some(k) => k,
        None => {
            let ctxt = local_data::get(super::ctxtkey, |x| *x.unwrap());
            debug!("could not find %? in defmap (`%s`)", id,
                   syntax::ast_map::node_id_to_str(ctxt.tycx.items, id, ctxt.sess.intr()));
            fail!("Unexpected failure: unresolved id not in defmap (this is a bug!)");
        }
    };

    Resolved(match *d {
        def_fn(i, _) => i.node,
        def_self(i, _) | def_self_ty(i) => return Self(i),
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
        def_ty_param(i, _) => return Generic(i.node),
        def_struct(i) => i.node,
        def_typaram_binder(i) => i,
        _ => fail!("resolved type maps to a weird def"),
    })
}

impl Clean<Type> for ast::Ty {
    pub fn clean(&self) -> Type {
        use syntax::ast::*;
        debug!("cleaning type `%?`", self);
        let codemap = local_data::get(super::ctxtkey, |x| *x.unwrap()).sess.codemap;
        debug!("span corresponds to `%s`", codemap.span_to_str(self.span));
        let mut t = match self.node {
            ty_nil => Unit,
            ty_ptr(ref m) | ty_rptr(_, ref m) => resolve_type(&m.ty.clean()),
            ty_box(ref m) => Managed(~resolve_type(&m.ty.clean())),
            ty_uniq(ref m) => Unique(~resolve_type(&m.ty.clean())),
            ty_vec(ref m) | ty_fixed_length_vec(ref m, _) => Vector(~resolve_type(&m.ty.clean())),
            ty_tup(ref tys) => Tuple(tys.iter().transform(|x| resolve_type(&x.clean())).collect()),
            ty_path(_, _, id) => Unresolved(id),
            _ => fail!("Unimplemented type (this is a bug)"),
        };
        resolve_type(&t)
    }
}

impl Clean<Enum> for doctree::Enum {
    pub fn clean(&self) -> Enum {
        Enum {
            variants: self.variants.iter().transform(|x| x.clean()).collect(),
            generics: self.generics.clean(),
            attrs: collapse_docs(self.attrs.iter().transform(|x| x.clean()).collect()),
            name: its(&self.name).to_owned(),
            where: self.where.clean(),
            node: self.id
        }
    }
}

impl Clean<Variant> for doctree::Variant {
    pub fn clean(&self) -> Variant {
        Variant {
            name: its(&self.name).to_owned(),
            attrs: collapse_docs(self.attrs.iter().transform(|x| x.clean()).collect()),
            kind: self.kind.clean(),
            visibility: self.visibility
        }
    }
}

impl Clean<VariantKind> for ast::variant_kind {
    pub fn clean(&self) -> VariantKind {
        match self {
            &ast::tuple_variant_kind(ref args) => {
                if args.len() == 0 {
                    CLikeVariant
                } else {
                    TupleVariant(args.iter().transform(|x| x.ty.clean()).collect())
                }
            },
            &ast::struct_variant_kind(ref sd) => StructVariant(sd.clean()),
        }
    }
}

impl Clean<Function> for doctree::Function {
    pub fn clean(&self) -> Function {
        Function {
            decl: self.decl.clean(),
            name: its(&self.name).to_owned(),
            id: self.id,
            attrs: collapse_docs(self.attrs.clean()),
            where: self.where.clean(),
            visibility: self.visibility,
            generics: self.generics.clean(),
        }
    }
}

impl Clean<VariantStruct> for syntax::ast::struct_def {
    pub fn clean(&self) -> VariantStruct {
        VariantStruct {
            struct_type: doctree::struct_type_from_def(self),
            fields: self.fields.iter().transform(
                                       |x| doctree::StructField::new(&x.node).clean()).collect()
        }
    }
}

impl Clean<~str> for syntax::codemap::span {
    pub fn clean(&self) -> ~str {
        let cm = local_data::get(super::ctxtkey, |x| x.unwrap().clone()).sess.codemap;
        cm.span_to_str(*self)
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

impl<T: Clean<U>, U> Clean<~[U]> for ~[T] {
    pub fn clean(&self) -> ~[U] {
        self.iter().transform(|x| x.clean()).collect()
    }
}
