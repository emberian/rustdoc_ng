//! This module contains the "cleaned" pieces of the AST, and the functions
//! that clean them.

use its = syntax::parse::token::ident_to_str;

use rustc::metadata::{csearch,decoder,cstore};
use syntax;
use syntax::ast;

use doctree;
use visit_ast;
use std::local_data;

pub trait Clean<T> {
    pub fn clean(&self) -> T;
}

impl<T: Clean<U>, U> Clean<~[U]> for ~[T] {
    pub fn clean(&self) -> ~[U] {
        self.iter().transform(|x| x.clean()).collect()
    }
}
impl<T: Clean<U>, U> Clean<U> for @T {
    pub fn clean(&self) -> U {
        (**self).clean()
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

#[deriving(Clone, Encodable, Decodable)]
pub struct Crate {
    name: ~str,
    attrs: ~[Attribute],
    module: Item<Module>,
}

impl Clean<Crate> for visit_ast::RustdocVisitor {
    pub fn clean(&self) -> Crate {
        use syntax::attr::{find_linkage_metas, last_meta_item_value_str_by_name};
        let maybe_meta = last_meta_item_value_str_by_name(find_linkage_metas(self.attrs), "name");

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

/// Anything with a source location and set of attributes and, optionally, a
/// name. That is, anything that can be documented. This doesn't correspond
/// directly to the AST's concept of an item; it's a strict superset.
#[deriving(Clone, Encodable, Decodable)]
pub struct Item<T> {
    /// Stringified span
    source: ~str,
    /// Not everything has a name. E.g., impls
    name: Option<~str>,
    attrs: ~[Attribute],
    inner: T,
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Module {
    structs: ~[Item<Struct>],
    enums: ~[Item<Enum>],
    fns: ~[Item<Function>],
    mods: ~[Item<Module>],
    typedefs: ~[Item<Typedef>],
    statics: ~[Item<Static>],
    traits: ~[Item<Trait>],
    impls: ~[Item<Impl>],
    view_items: ~[Item<ViewItem>],
}

impl Clean<Item<Module>> for doctree::Module {
    pub fn clean(&self) -> Item<Module> {
        let name = if self.name.is_some() {
            self.name.unwrap().clean()
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

#[deriving(Clone, Encodable, Decodable)]
pub enum Attribute {
    Word(~str),
    List(~str, ~[Attribute]),
    NameValue(~str, ~str)
}

impl Clean<Attribute> for ast::MetaItem {
    pub fn clean(&self) -> Attribute {
        match self.node {
            ast::MetaWord(s) => Word(s.to_owned()),
            ast::MetaList(ref s, ref l) => List(s.to_owned(), l.clean()),
            ast::MetaNameValue(s, ref v) => NameValue(s.to_owned(), lit_to_str(v))
        }
    }
}

impl Clean<Attribute> for ast::Attribute {
    pub fn clean(&self) -> Attribute {
        self.node.value.clean()
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct TyParam {
    name: ~str,
    id: ast::NodeId,
    bounds: ~[TyParamBound]
}

impl Clean<TyParam> for ast::TyParam {
    pub fn clean(&self) -> TyParam {
        TyParam {
            name: self.ident.clean(),
            id: self.id,
            bounds: self.bounds.iter().transform(|x| x.clean()).collect()
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub enum TyParamBound {
    RegionBound,
    TraitBound(TraitRef)
}

impl Clean<TyParamBound> for ast::TyParamBound {
    pub fn clean(&self) -> TyParamBound {
        match *self {
            ast::RegionTyParamBound => RegionBound,
            ast::TraitTyParamBound(ref t) => TraitBound(t.clean()),
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Lifetime(~str);

impl Clean<Lifetime> for ast::Lifetime {
    pub fn clean(&self) -> Lifetime {
        Lifetime(self.ident.clean())
    }
}

// maybe use a Generic enum and use ~[Generic]?
#[deriving(Clone, Encodable, Decodable)]
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

#[deriving(Clone, Encodable, Decodable)]
pub struct Method {
    generics: Generics,
    self_: SelfTy,
    purity: ast::purity,
    decl: FnDecl,
    id: ast::NodeId,
    vis: Visibility,
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
    purity: ast::purity,
    decl: FnDecl,
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

#[deriving(Clone, Encodable, Decodable)]
pub enum SelfTy {
    SelfStatic,
    SelfValue,
    SelfBorrowed(Option<Lifetime>, Mutability),
    SelfManaged(Mutability),
    SelfOwned,
}

impl Clean<SelfTy> for ast::explicit_self {
    pub fn clean(&self) -> SelfTy {
        match self.node {
            ast::sty_static => SelfStatic,
            ast::sty_value => SelfValue,
            ast::sty_uniq => SelfOwned,
            ast::sty_region(lt, mt) => SelfBorrowed(lt.clean(), mt.clean()),
            ast::sty_box(mt) => SelfManaged(mt.clean()),
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Function {
    decl: FnDecl,
    visibility: Visibility,
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

#[deriving(Clone, Encodable, Decodable)]
pub struct FnDecl {
    inputs: ~[Argument],
    output: Type,
    cf: RetStyle,
    attrs: ~[Attribute]
}

impl Clean<FnDecl> for ast::fn_decl {
    pub fn clean(&self) -> FnDecl {
        FnDecl {
            inputs: self.inputs.iter().transform(|x| x.clean()).collect(),
            output: (self.output.clean()),
            cf: self.cf.clean(),
            attrs: ~[]
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Argument {
    ty: Type,
    name: ~str,
    id: ast::NodeId
}

impl Clean<Argument> for ast::arg {
    pub fn clean(&self) -> Argument {
        Argument {
            name: name_from_pat(self.pat),
            ty: (self.ty.clean()),
            id: self.id
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

#[deriving(Clone, Encodable, Decodable)]
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

#[deriving(Clone, Encodable, Decodable)]
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

impl Clean<TraitMethod> for ast::trait_method {
    pub fn clean(&self) -> TraitMethod {
        match self {
            &ast::required(ref t) => Required(t.clean()),
            &ast::provided(ref t) => Provided(t.clean()),
        }
    }
}

/// A representation of a Type suitable for hyperlinking purposes. Ideally one can get the original
/// type out of the AST/ty::ctxt given one of these, if more information is needed. Most importantly
/// it does not preserve mutability or boxes.
#[deriving(Clone, Encodable, Decodable)]
pub enum Type {
    /// Most types start out as "Unresolved". It serves as an intermediate stage between cleaning
    /// and type resolution.
    Unresolved(ast::NodeId),
    /// structs/enums/traits (anything that'd be an ast::ty_path)
    Resolved(ast::NodeId),
    /// Reference to an item in an external crate (fully qualified path)
    External(~str, ~str),
    /// For parameterized types, so the consumer of the JSON don't go looking
    /// for types which don't exist anywhere.
    Generic(ast::NodeId),
    /// For references to self
    Self(ast::NodeId),
    /// Primitives are just the fixed-size numeric types (plus int/uint/float), and char.
    Primitive(ast::prim_ty),
    Closure(~ClosureDecl),
    /// extern "ABI" fn
    BareFunction(~BareFunctionDecl),
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
    RawPointer(~Type),
    BorrowedRef(~Type),
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
            ty_ptr(ref m) =>  RawPointer(~resolve_type(&m.ty.clean())),
            ty_rptr(_, ref m) => BorrowedRef(~resolve_type(&m.ty.clean())),
            ty_box(ref m) => Managed(~resolve_type(&m.ty.clean())),
            ty_uniq(ref m) => Unique(~resolve_type(&m.ty.clean())),
            ty_vec(ref m) | ty_fixed_length_vec(ref m, _) => Vector(~resolve_type(&m.ty.clean())),
            ty_tup(ref tys) => Tuple(tys.iter().transform(|x| resolve_type(&x.clean())).collect()),
            ty_path(_, _, id) => Unresolved(id),
            ty_closure(ref c) => Closure(~c.clean()),
            ty_bare_fn(ref barefn) => BareFunction(~barefn.clean()),
            ty_bot => Bottom,
            ref x => fail!("Unimplemented type %?", x),
        };
        resolve_type(&t)
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct StructField {
    type_: Type,
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

#[deriving(Clone, Encodable, Decodable)]
pub struct Struct {
    id: ast::NodeId,
    struct_type: doctree::StructType,
    generics: Generics,
    fields: ~[Item<StructField>],
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
#[deriving(Clone, Encodable, Decodable)]
pub struct VariantStruct {
    struct_type: doctree::StructType,
    fields: ~[Item<StructField>],
}

impl Clean<VariantStruct> for syntax::ast::struct_def {
    pub fn clean(&self) -> VariantStruct {
        VariantStruct {
            struct_type: doctree::struct_type_from_def(self),
            fields: self.fields.clean(),
        }
    }
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Enum {
    variants: ~[Item<Variant>],
    generics: Generics,
    id: ast::NodeId,
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

#[deriving(Clone, Encodable, Decodable)]
pub struct Variant {
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

#[deriving(Clone, Encodable, Decodable)]
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

fn name_from_pat(p: &ast::pat) -> ~str {
    use syntax::ast::*;
    match p.node {
        pat_wild => ~"_",
        pat_ident(_, ref p, _) => path_to_str(p),
        pat_enum(ref p, _) => path_to_str(p),
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
    for char in s.iter() {
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
    for at in attrs.iter() {
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
        _ => return (*t).clone(),
    };

    let dm = local_data::get(super::ctxtkey, |x| *x.unwrap()).tycx.def_map;
    debug!("searching for %? in defmap", id);
    let d = match dm.find(&id) {
        Some(k) => k,
        None => {
            let ctxt = local_data::get(super::ctxtkey, |x| *x.unwrap());
            debug!("could not find %? in defmap (`%s`)", id,
                   syntax::ast_map::node_id_to_str(ctxt.tycx.items, id, ctxt.sess.intr()));
            fail!("Unexpected failure: unresolved id not in defmap (this is a bug!)")
        }
    };

    let def_id = match *d {
        def_fn(i, _) => i,
        def_self(i, _) | def_self_ty(i) => return Self(i),
        def_ty(i) => i,
        def_trait(i) => {
            debug!("saw def_trait in def_to_id");
            i
        },
        def_prim_ty(p) => match p {
            ty_str => return String,
            ty_bool => return Bool,
            _ => return Primitive(p)
        },
        def_ty_param(i, _) => return Generic(i.node),
        def_struct(i) => i,
        def_typaram_binder(i) => return Resolved(i),
        x => fail!("resolved type maps to a weird def %?", x),
    };

    if def_id.crate != ast::CRATE_NODE_ID {
        let sess = local_data::get(super::ctxtkey, |x| *x.unwrap()).sess;
        let mut path = ~"";
        let mut ty = ~"";
        do csearch::each_path(sess.cstore, def_id.crate) |pathstr, deflike, _vis| {
            match deflike {
                decoder::dl_def(di) => {
                    let d2 = match di {
                        def_fn(i, _) | def_ty(i) | def_trait(i) |
                            def_struct(i) | def_mod(i) => Some(i),
                        _ => None,
                    };
                    if d2.is_some() {
                        let d2 = d2.unwrap();
                        if def_id.node == d2.node {
                            debug!("found external def: %?", di);
                            path = pathstr.to_owned();
                            ty = match di {
                                def_fn(*) => ~"fn",
                                def_ty(*) => ~"enum",
                                def_trait(*) => ~"trait",
                                def_prim_ty(p) => match p {
                                    ty_str => ~"str",
                                    ty_bool => ~"bool",
                                    ty_int(t) => match t.to_str() {
                                        ~"" => ~"i",
                                        s => s
                                    },
                                    ty_uint(t) => t.to_str(),
                                    ty_float(t) => t.to_str()
                                },
                                def_ty_param(*) => ~"generic",
                                def_struct(*) => ~"struct",
                                def_typaram_binder(*) => ~"typaram_binder",
                                x => fail!("resolved external maps to a weird def %?", x),
                            };

                        }
                    }
                },
                _ => (),
            };
            true
        };
        let cname = cstore::get_crate_data(sess.cstore, def_id.crate).name.to_owned();
        External(cname + "::" + path, ty)
    } else {
        Resolved(def_id.node)
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
