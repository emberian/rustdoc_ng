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

pub struct Crate {
    name: ~str,
    attrs: ~[Attribute],
    mods: ~[Module],
}

impl Clean<Crate> for visit::RustdocVisitor {
    pub fn clean(&self) -> Crate {
        use syntax::attr::{find_linkage_metas, last_meta_item_value_str_by_name};
        Crate {
            name: match maybe_meta {
                Some(x) => x.to_owned(),
                None => fail!("rustdoc_ng requires a #[link(name=\"foo\")] crate attribute"),
            },
            module: self.module.clean(),
            attrs: collapse_docs(self.attrs.clean()),
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
        Item {
            name: Some(name),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Module {
                structs    : self.structs.clean(),
                enums      : self.enums.clean(),
                fns        : self.fns.clean(),
                mods       : self.mods.clean(),
                typedefs   : self.typedefs.clean(),
                statics    : self.statics.clean(),
                traits     : self.traits.clean(),
                impls      : self.impls.clean(),
                view_items : self.view_items.clean(),
            }
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

pub struct TyParam {
    name: ~str,
    node: ast::node_id,
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

impl Clean<TyParamBound> for ast::TyParamBound {
    pub fn clean(&self) -> TyParamBound {
        match *self {
            ast::RegionTyParamBound => RegionBound,
            ast::TraitTyParamBound(_) => TraitBound(Trait::new())
        }
    }
}

pub struct Lifetime(~str);

impl Clean<Lifetime> for ast::Lifetime {
    pub fn clean(&self) -> Lifetime {
        Lifetime(its(&self.ident).to_owned())
    }
}

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
    id: ast::node_id,
    vis: Visibility
}

impl Clean<Item<Method>> for ast::method {
    pub fn clean(&self) -> Item<Method> {
        Item {
            name: Some(self.ident.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.span.clean(),
            inner: Method {
                generics: self.generics.clean(),
                self_: self.explicit_self.clean(),
                purity: self.purity.clone(),
                decl: self.decl.clean(),
                id: self.self_id.clone(),
                vis: self.vis,
            }
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct TyMethod {
    ident: ~str,
    attrs: ~[Attribute],
    generics: Generics,
    id: ast::NodeId,
    self_: SelfTy,
}

impl Clean<Item<TyMethod>> for ast::TypeMethod {
    pub fn clean(&self) -> Item<TyMethod> {
        Item {
            name: Some(self.ident.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.span.clean(),
            inner: TyMethod {
                purity: self.purity.clone(),
                decl: self.decl.clean(),
                self_: self.explicit_self.clean(),
                generics: self.generics.clean(),
                id: self.id,
            }
        }
    }
}

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
}

impl Clean<Item<Function>> for doctree::Function {
    pub fn clean(&self) -> Item<Function> {
        Item {
            name: Some(self.name.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Function {
                id: self.id,
                decl: self.decl.clean(),
                visibility: self.visibility,
                generics: self.generics.clean(),
            }
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct ClosureDecl {
    sigil: ast::Sigil,
    region: Option<Lifetime>,
    lifetimes: ~[Lifetime],
    decl: FnDecl,
    onceness: ast::Onceness,
    purity: ast::purity,
    bounds: ~[TyParamBound]
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


pub struct FnDecl {
    inputs: ~[Argument],
    output: @Type,
    cf: RetStyle,
    attrs: ~[Attribute]
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


pub struct Argument {
    mutable: bool,
    ty: @Type,
    //TODO pat
    id: ast::node_id
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

pub struct Trait {
    methods: ~[TraitMethod],
    generics: Generics,
    parents: ~[TraitRef],
    id: ast::NodeId,
}

impl Clean<Item<Trait>> for doctree::Trait {
    pub fn clean(&self) -> Item<Trait> {
        Item {
            name: Some(self.name.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Trait {
                methods: self.methods.clean(),
                generics: self.generics.clean(),
                parents: self.parents.clean(),
                id: self.id
            }
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct TraitRef {
    path: Path,
    id: ast::NodeId,
}

impl Clean<TraitRef> for ast::trait_ref {
    pub fn clean(&self) -> TraitRef {
        TraitRef {
            path: self.path.clean(),
            id: self.ref_id,
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub enum TraitMethod {
    Required(Item<TyMethod>),
    Provided(Item<Method>),
}

impl TraitMethod {
    pub fn is_req(&self) -> bool {
        match self {
            &Required(*) => true,
            _ => false,
        }
    }
    pub fn is_def(&self) -> bool {
        match self {
            &Provided(*) => true,
            _ => false,
        }
    }
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

pub struct StructField {
    name: ~str,
    type_: Type,
    attrs: ~[Attribute],
    visibility: Option<Visibility>,
}

impl Clean<Item<StructField>> for ast::struct_field {
    pub fn clean(&self) -> Item<StructField> {
        let (name, vis) = match self.node.kind {
            ast::named_field(id, vis) => (Some(id), Some(vis)),
            _ => (None, None)
        };
        Item {
            name: name.clean(),
            attrs: collapse_docs(self.node.attrs.clean()),
            source: self.span.clean(),
            inner: StructField {
                type_: self.node.ty.clean(),
                visibility: vis,
            }
        }
    }
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

impl Clean<Item<Struct>> for doctree::Struct {
    pub fn clean(&self) -> Item<Struct> {
        Item {
            name: Some(self.name.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Struct {
                id: self.id,
                struct_type: self.struct_type,
                generics: self.generics.clean(),
                fields: self.fields.clean(),
            }
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
    node: ast::node_id,
    where: ~str,
}

impl Clean<Item<Enum>> for doctree::Enum {
    pub fn clean(&self) -> Item<Enum> {
        Item {
            name: Some(self.name.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Enum {
                variants: self.variants.iter().transform(|x| x.clean()).collect(),
                generics: self.generics.clean(),
                id: self.id,
            }
        }
    }
}

pub struct Variant {
    name: ~str,
    attrs: ~[Attribute],
    kind: VariantKind,
    visibility: Visibility,
}

impl Clean<Item<Variant>> for doctree::Variant {
    pub fn clean(&self) -> Item<Variant> {
        Item {
            name: Some(self.name.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Variant {
                kind: self.kind.clean(),
                visibility: self.visibility
            }
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

#[deriving(Clone, Encodable, Decodable)]
pub struct Path {
    name: ~str,
    lifetime: Option<Lifetime>,
    typarams: ~[Type]
}

impl Clean<Path> for ast::Path {
    pub fn clean(&self) -> Path {
        Path {
            name: path_to_str(self),
            lifetime: self.rp.clean(),
            typarams: self.types.clean(),
        }
    }
}

pub fn path_to_str(p: &ast::Path) -> ~str {
    use syntax::parse::token::interner_get;

    let mut s = ~"";
    let mut first = true;
    for i in p.idents.iter().transform(|x| interner_get(x.name)) {
        if !first || p.global {
            s.push_str("::");
        } else {
            first = false;
        }
        s.push_str(i);
    }
    s
}

impl Clean<~str> for ast::ident {
    pub fn clean(&self) -> ~str {
        its(self).to_owned()
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Typedef {
    type_: Type,
    generics: Generics,
    id: ast::NodeId,
}

impl Clean<Item<Typedef>> for doctree::Typedef {
    pub fn clean(&self) -> Item<Typedef> {
        Item {
            name: Some(self.name.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Typedef {
                type_: self.ty.clean(),
                generics: self.gen.clean(),
                id: self.id.clone(),
            }
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct BareFunctionDecl {
    purity: ast::purity,
    generics: Generics,
    decl: FnDecl,
    abi: ~str
}

impl Clean<BareFunctionDecl> for ast::TyBareFn {
    pub fn clean(&self) -> BareFunctionDecl {
        BareFunctionDecl {
            purity: self.purity,
            generics: Generics {
                lifetimes: self.lifetimes.clean(),
                type_params: ~[],
            },
            decl: self.decl.clean(),
            abi: self.abis.to_str(),
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Static {
    type_: Type,
    mutability: Mutability,
    /// It's useful to have the value of a static documented, but I have no
    /// desire to represent expressions (that'd basically be all of the AST,
    /// which is huge!). So, have a string.
    expr: ~str,
}

impl Clean<Item<Static>> for doctree::Static {
    pub fn clean(&self) -> Item<Static> {
        debug!("claning static %s: %?", self.name.clean(), self);
        Item {
            name: Some(self.name.clean()),
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Static {
                type_: self.type_.clean(),
                mutability: self.mutability.clean(),
                expr: self.expr.span.to_src(),
            }
        }
    }
}

#[deriving(ToStr, Clone, Encodable, Decodable)]
pub enum Mutability {
    Mutable,
    Immutable,
    Const,
}

impl Clean<Mutability> for ast::mutability {
    pub fn clean(&self) -> Mutability {
        match self {
            &ast::m_mutbl => Mutable,
            &ast::m_imm => Immutable,
            &ast::m_const => Const
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Impl {
    generics: Generics,
    trait_: Option<TraitRef>,
    for_: Type,
    methods: ~[Item<Method>],
}

impl Clean<Item<Impl>> for doctree::Impl {
    pub fn clean(&self) -> Item<Impl> {
        Item {
            name: None,
            attrs: collapse_docs(self.attrs.clean()),
            source: self.where.clean(),
            inner: Impl {
                generics: self.generics.clean(),
                trait_: self.trait_.clean(),
                for_: self.for_.clean(),
                methods: self.methods.clean(),
            }
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct ViewItem {
    vis: Visibility,
    inner: ViewItemInner
}

impl Clean<Item<ViewItem>> for ast::view_item {
    pub fn clean(&self) -> Item<ViewItem> {
        Item {
            name: None,
            attrs: collapse_docs(self.attrs.clean()),
            source: self.span.clean(),
            inner: ViewItem {
                vis: self.vis,
                inner: self.node.clean()
            }
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub enum ViewItemInner {
    ExternMod(~str, ~[Attribute], ast::NodeId),
    Import(~[ViewPath])
}

impl Clean<ViewItemInner> for ast::view_item_ {
    pub fn clean(&self) -> ViewItemInner {
        match self {
            &ast::view_item_extern_mod(ref i, ref mi, ref id) =>
                ExternMod(i.clean(), mi.clean(), *id),
            &ast::view_item_use(ref vp) => Import(vp.clean())
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub enum ViewPath {
    SimpleImport(~str, Path, ast::NodeId),
    GlobImport(Path, ast::NodeId),
    ImportList(Path, ~[ViewListIdent], ast::NodeId)
}

impl Clean<ViewPath> for ast::view_path {
    pub fn clean(&self) -> ViewPath {
        match self.node {
            ast::view_path_simple(ref i, ref p, ref id) => SimpleImport(i.clean(), p.clean(), *id),
            ast::view_path_glob(ref p, ref id) => GlobImport(p.clean(), *id),
            ast::view_path_list(ref p, ref pl, ref id) => ImportList(p.clean(), pl.clean(), *id),
        }
    }
}

pub type ViewListIdent = ~str;

impl Clean<ViewListIdent> for ast::path_list_ident {
    pub fn clean(&self) -> ViewListIdent {
        self.node.name.clean()
    }
}

// Utilities

trait ToSource {
    pub fn to_src(&self) -> ~str;
}

impl ToSource for syntax::codemap::span {
    pub fn to_src(&self) -> ~str {
        let cm = local_data::get(super::ctxtkey, |x| x.unwrap().clone()).sess.codemap.clone();
        match cm.span_to_snippet(*self) {
            Some(x) => x,
            None    => ~""
        }
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
            (Stripped, ' ') => { res.push_char(char); state = Collect; }
            (Stripped, '\t') => { res.push_char(char); state = Collect; }
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
