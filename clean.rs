use its = syntax::parse::token::ident_to_str;

use syntax;
use syntax::ast;

use doctree;
use visit;
use std::local_data;

pub trait Clean<T> {
    pub fn clean(&self) -> T;
}

impl<T: Clean<U>, U> Clean<~[U]> for ~[T] {
    pub fn clean(&self) -> ~[U] {
        self.iter().transform(|x| x.clean()).collect()
    }
}

impl<T: Clean<U>, U> Clean<Option<U>> for Option<T> {
    pub fn clean(&self) -> Option<U> {
        match self {
            &None => None,
            &Some(ref v) => Some(v.clean())
        }
    }
}

impl<T: Clean<U>, U> Clean<~[U]> for syntax::opt_vec::OptVec<T> {
    pub fn clean(&self) -> ~[U] {
        match self {
            &syntax::opt_vec::Empty => ~[],
            &syntax::opt_vec::Vec(ref v) => v.clean()
        }
    }
}

pub struct Crate {
    name: ~str,
    attrs: ~[Attribute],
    mods: ~[Module],
}

impl Clean<Crate> for visit::RustdocVisitor {
    pub fn clean(&self) -> Crate {
        use syntax::attr::{find_linkage_metas, last_meta_item_value_str_by_name};
        let maybe_meta = last_meta_item_value_str_by_name(find_linkage_metas(self.attrs), "name");

        Crate {
            name: match maybe_meta {
                Some(x) => x.to_owned(),
                None => fail!("rustdoc_ng requires a #[link(name=\"foo\")] crate attribute"),
            },
            mods: self.mods.clean(),
            attrs: self.attrs.clean(),
        }
    }
}

pub struct Module {
    name: ~str,
    attrs: ~[Attribute],
    structs: ~[Struct],
    enums: ~[Enum],
    fns: ~[Function],
    mods: ~[Module],
}

impl Clean<Module> for doctree::Module {
    pub fn clean(&self) -> Module {
        let name = if self.name.is_some() {
            its(&self.name.unwrap()).to_owned()
        } else {
            ~""
        };
        Module {
            name: name,
            attrs: self.attrs.clean(),
            structs: self.structs.clean(),
            enums: self.enums.clean(),
            fns: self.fns.clean(),
            mods: self.mods.clean(),
        }
    }
}

#[deriving(Clone)]
pub enum Attribute {
    Word(~str),
    List(~str, ~[Attribute]),
    NameValue(~str, ~str)
}

impl Clean<Attribute> for ast::MetaItem_ {
    pub fn clean(&self) -> Attribute {
        match *self {
            ast::MetaWord(s) => Word(remove_comment_tags(s)),
            ast::MetaList(ref s, ref l) => List(remove_comment_tags(*s), l.iter()
                                         .transform(|x| x.node.clean()).collect()),
            ast::MetaNameValue(s, ref v) => NameValue(remove_comment_tags(s),
                                         remove_comment_tags(lit_to_str(v)))
        }
    }
}

impl Clean<Attribute> for ast::Attribute {
    pub fn clean(&self) -> Attribute {
        self.node.value.node.clean()
    }
}

#[deriving(Clone)]
pub struct TyParam {
    name: ~str,
    node: ast::NodeId,
    bounds: ~[TyParamBound]
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

pub enum TyParamBound {
    RegionBound,
    TraitBound(Trait)
}

#[doc = "Automatically derived."]
impl ::std::clone::Clone for TyParamBound {
    pub fn clone(&self) -> TyParamBound {
        match *self {
            RegionBound => RegionBound,
            TraitBound(ref __self_0) => TraitBound((*__self_0).clone())
        }
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

#[deriving(Clone)]
pub struct Lifetime(~str);

impl Clean<Lifetime> for ast::Lifetime {
    pub fn clean(&self) -> Lifetime {
        Lifetime(its(&self.ident).to_owned())
    }
}

// maybe use a Generic enum and use ~[Generic]?
#[deriving(Clone)]
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

impl Clean<Generics> for ast::Generics {
    pub fn clean(&self) -> Generics {
        Generics {
            lifetimes: self.lifetimes.iter().transform(|x| x.clean()).collect(),
            type_params: self.ty_params.iter().transform(|x| x.clean()).collect()
        }
    }
}

pub struct Method {
    ident: ~str,
    attrs: ~[Attribute],
    generics: Generics,
    //explicit_self: ExplicitSelf,
    id: ast::NodeId,
    vis: Visibility
}

impl ::std::clone::Clone for Method {
    pub fn clone(&self) -> Method {
        match *self {
            Method{ident: ref __self_0_0,
            attrs: ref __self_0_1,
            generics: ref __self_0_2,
            id: ref __self_0_3,
            vis: ref __self_0_4} =>
                Method{ident: __self_0_0.clone(),
                attrs: __self_0_1.clone(),
                generics: (*__self_0_2).clone(),
                id: __self_0_3.clone(),
                vis: __self_0_4.clone(),}
        }
    }
}


pub struct TyMethod {
    ident: ~str,
    attrs: ~[Attribute],
    generics: Generics,
    id: ast::NodeId,

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
    id: ast::NodeId,
    attrs: ~[Attribute]
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

#[deriving(Clone)]
pub struct ClosureDecl {
    sigil: ast::Sigil,
    region: Option<Lifetime>,
    lifetimes: ~[Lifetime],
    decl: FnDecl,
    onceness: ast::Onceness,
    purity: ast::purity,
    bounds: ~[TyParamBound]
}

impl Clean<ClosureDecl> for ast::TyClosure {
    pub fn clean(&self) -> ClosureDecl {
        ClosureDecl {
            sigil: self.sigil,
            region: self.region.clean(),
            lifetimes: self.lifetimes.clean(),
            decl: self.decl.clean(),
            onceness: self.onceness,
            purity: self.purity,
            bounds: match self.bounds {
                Some(ref x) => x.clean(),
                None        => ~[]
            },
        }
    }
}

pub struct FnDecl {
    inputs: ~[Argument],
    output: @Type,
    cf: RetStyle,
    attrs: ~[Attribute]
}

#[doc = "Automatically derived."]
impl ::std::clone::Clone for FnDecl {
    pub fn clone(&self) -> FnDecl {
        match *self {
            FnDecl{inputs: ref __self_0_0,
            output: ref __self_0_1,
            cf: ref __self_0_2,
            attrs: ref __self_0_3} => FnDecl{
                inputs: __self_0_0.clone(),
                output: __self_0_1.clone(),
                cf: (*__self_0_2).clone(),
                attrs: __self_0_3.clone(),
            }
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

#[deriving(Clone)]
pub struct Argument {
    ty: @Type,
    name: ~str,
    id: ast::NodeId
}

impl Clean<Argument> for ast::arg {
    pub fn clean(&self) -> Argument {
        Argument {
            name: name_from_pat(self.pat),
            ty: @(self.ty.clean()),
            id: self.id
        }
    }
}

#[deriving(Clone)]
pub enum RetStyle {
    NoReturn,
    Return
}

impl Clean<RetStyle> for ast::ret_style {
    pub fn clean(&self) -> RetStyle {
        match *self {
            ast::return_val => Return,
            ast::noreturn => NoReturn
        }
    }
}

#[deriving(Clone)]
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
    Unresolved(ast::NodeId),
    /// structs/enums/traits (anything that'd be an ast::ty_path)
    Resolved(ast::NodeId),
    /// For parameterized types, so the consumer of the JSON don't go looking
    /// for types which don't exist anywhere.
    Generic(ast::NodeId),
    /// For references to self
    Self(ast::NodeId),
    /// Primitives are just the fixed-size numeric types (plus int/uint/float), and char.
    Primitive(ast::prim_ty),
    Closure(~ClosureDecl),
    Tuple(~[Type]),
    Vector(~Type),
    String,
    Bool,
    /// aka ty_nil
    Unit,
    /// aka ty_bot
    Bottom,
    Unique(~Type),
    Managed(~Type),
    // region, raw, other boxes, mutable
}

impl Clean<Type> for ast::Ty {
    pub fn clean(&self) -> Type {
        use syntax::ast::*;
        debug!("cleaning type `%?`", self);
        let codemap = local_data::get(super::ctxtkey, |x| *x.unwrap()).sess.codemap;
        debug!("span corresponds to `%s`", codemap.span_to_str(self.span));
        let t = match self.node {
            ty_nil => Unit,
            ty_ptr(ref m) | ty_rptr(_, ref m) => resolve_type(&m.ty.clean()),
            ty_box(ref m) => Managed(~resolve_type(&m.ty.clean())),
            ty_uniq(ref m) => Unique(~resolve_type(&m.ty.clean())),
            ty_vec(ref m) | ty_fixed_length_vec(ref m, _) => Vector(~resolve_type(&m.ty.clean())),
            ty_tup(ref tys) => Tuple(tys.iter().transform(|x| resolve_type(&x.clean())).collect()),
            ty_path(_, _, id) => Unresolved(id),
            ty_closure(ref c) => Closure(~c.clean()),
            ty_bot => Bottom,
            ref x => fail!("Unimplemented type %?", x),
        };
        resolve_type(&t)
    }
}

pub struct StructField {
    name: ~str,
    type_: Type,
    attrs: ~[Attribute],
    visibility: Option<Visibility>,
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

pub type Visibility = ast::visibility;

pub struct Struct {
    name: ~str,
    where: ~str,
    node: ast::NodeId,
    struct_type: doctree::StructType,
    attrs: ~[Attribute],
    generics: Generics,
    fields: ~[StructField],
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

/// This is a more limited form of the standard Struct, different in that it
/// it lacks the things most items have (name, id, parameterization). Found
/// only as a variant in an enum.
pub struct VariantStruct {
    struct_type: doctree::StructType,
    fields: ~[StructField],
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

pub struct Enum {
    variants: ~[Variant],
    generics: Generics,
    attrs: ~[Attribute],
    name: ~str,
    node: ast::NodeId,
    where: ~str,
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

pub struct Variant {
    name: ~str,
    attrs: ~[Attribute],
    kind: VariantKind,
    visibility: Visibility,
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

pub enum VariantKind {
    CLikeVariant,
    TupleVariant(~[Type]),
    StructVariant(VariantStruct),
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

impl Clean<~str> for syntax::codemap::span {
    pub fn clean(&self) -> ~str {
        let cm = local_data::get(super::ctxtkey, |x| x.unwrap().clone()).sess.codemap;
        cm.span_to_str(*self)
    }
}

impl Clean<~str> for ast::Path {
    pub fn clean(&self) -> ~str {
        use syntax::parse::token::interner_get;

        let mut s = ~"";
        for self.idents.iter().transform(|x| interner_get(x.name)).advance|i| {
            s.push_str(i);
        }
        s
    }
}
// Utility functions

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

fn name_from_pat(p: &ast::pat) -> ~str {
    use syntax::ast::*;
    match p.node {
        pat_wild => ~"_",
        pat_ident(_, ref p, _) => p.clean(),
        pat_enum(ref p, _) => p.clean(),
        pat_struct(*) => fail!("tried to get argument name from pat_struct, \
                                 which is not allowed in function arguments"),
        pat_tup(*) => ~"(tuple arg NYI)",
        pat_box(p) => name_from_pat(p),
        pat_uniq(p) => name_from_pat(p),
        pat_region(p) => name_from_pat(p),
        pat_lit(*) => fail!("tried to get argument name from pat_lit, \
                             which is not allowed in function arguments"),
        pat_range(*) => fail!("tried to get argument name from pat_range, \
                               which is not allowed in function arguments"),
        pat_vec(*) => fail!("tried to get argument name from pat_vec, \
                             which is not allowed in function arguments")
    }
}

fn remove_comment_tags(s: &str) -> ~str {
    if s.starts_with("/") {
        match s.slice(0,3) {
            &"///" => return s.slice(3, s.len()).trim().to_owned(),
            &"/**" | &"/*!" => return s.slice(3, s.len() - 2).trim().to_owned(),
            _ => return s.trim().to_owned()
        }
    } else {
        return s.to_owned();
    }
}

enum CleanCommentStates {
    Collect,
    Strip,
    Stripped,
}

fn clean_comment_body(s: ~str) -> ~str {
    let mut res = ~"";
    let mut state = Strip;

    for s.iter().advance |char| {
        match (state, char) {
            (Strip, '*') => state = Stripped,
            (Strip, '/') => state = Stripped,
            (Stripped, '/') => state = Stripped,
            (Strip, ' ') => (),
            (Strip, '\t') => (),
            (Stripped, ' ') => state = Collect,
            (Stripped, '\t') => state = Collect,
            (_, '\n') => { res.push_char(char); state = Strip; }
            (_, char) => res.push_char(char)
        }
    }

    res = res.trim().to_owned();
    res.push_char('\n');
    res
}

pub fn collapse_docs(attrs: ~[Attribute]) -> ~[Attribute] {
    let mut docstr = ~"";
    for attrs.iter().advance |at| {
        match *at {
            //XXX how should these be separated?
            NameValue(~"doc", ref s) => docstr.push_str(fmt!("%s ", clean_comment_body(s.clone()))),
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

#[cfg(test)]
mod tests {
    use super::NameValue;

    #[test]
    fn test_doc_collapsing() {
        assert_eq!(collapse_docs(~"// Foo\n//Bar\n // Baz\n"), ~"Foo\nBar\nBaz");
        assert_eq!(collapse_docs(~"* Foo\n *  Bar\n *Baz\n"), ~"Foo\n Bar\nBaz");
        assert_eq!(collapse_docs(~"* Short desc\n *\n * Bar\n *Baz\n"), ~"Short desc\n\nBar\nBaz");
        assert_eq!(collapse_docs(~" * Foo"), ~"Foo");
        assert_eq!(collapse_docs(~"\n *\n *\n * Foo"), ~"Foo");
    }

    fn collapse_docs(input: ~str) -> ~str {
        let attrs = ~[NameValue(~"doc", input)];
        let attrs_clean = super::collapse_docs(attrs);

        match attrs_clean[0] {
            NameValue(~"doc", s) => s,
            _ => (fail!("dude where's my doc?"))
        }
    }
}
