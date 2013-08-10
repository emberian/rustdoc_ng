//! Rust AST Visitor. Extracts useful information and massages it into a form
//! usable for clean

use std::vec;

use syntax::abi::AbiSet;
use syntax::{ast, ast_map};
use syntax::codemap::span;

use doctree::*;
use std::local_data;

pub struct RustdocVisitor {
    module: Module,
    attrs: ~[ast::Attribute],
}

impl RustdocVisitor {
    pub fn new() -> RustdocVisitor {
        RustdocVisitor {
            module: Module::new(None),
            attrs: ~[],
        }
    }
}

impl RustdocVisitor {
    pub fn visit(@mut self, crate: &ast::Crate) {
        self.attrs = crate.attrs.clone();
        fn visit_struct_def(item: &ast::item, sd: @ast::struct_def, generics:
                            &ast::Generics) -> Struct {
            debug!("Visiting struct");
            let struct_type = struct_type_from_def(sd);
            Struct {
                id: item.id,
                struct_type: struct_type,
                name: item.ident,
                attrs: item.attrs.clone(),
                generics: generics.clone(),
                fields: sd.fields.iter().transform(|x| (*x).clone()).to_owned_vec(),
                where: item.span
            }
        }

        fn visit_enum_def(it: &ast::item, def: &ast::enum_def, params: &ast::Generics) -> Enum {
            debug!("Visiting enum");
            let mut vars: ~[Variant] = ~[];
            for x in def.variants.iter() {
                vars.push(Variant {
                    name: x.node.name,
                    attrs: x.node.attrs.clone(),
                    visibility: x.node.vis,
                    kind: x.node.kind.clone(),
                    where: x.span,
                });
            }
            Enum {
                name: it.ident,
                variants: vars,
                generics: params.clone(),
                attrs: it.attrs.clone(),
                id: it.id,
                where: it.span,
            }
        }

        fn visit_fn(item: &ast::item, fd: &ast::fn_decl, _purity: &ast::purity,
                     _abi: &AbiSet, gen: &ast::Generics) -> Function {
            debug!("Visiting fn");
            Function {
                id: item.id,
                attrs: item.attrs.clone(),
                decl: fd.clone(),
                name: item.ident,
                visibility: item.vis,
                where: item.span,
                generics: gen.clone(),
            }
        }

        // Only run on the toplevel mod(s)
        fn visit_mod(m: &ast::_mod, attrs: ~[ast::Attribute], span: span, id:
                     ast::NodeId, rcx: @mut RustdocVisitor) {
            rcx.module = visit_mod_contents(m, attrs, span, id);
        }

        fn visit_mod_contents(m: &ast::_mod, attrs: ~[ast::Attribute], span:
                              span, id: ast::NodeId) -> Module {
            let am = local_data::get(super::ctxtkey, |x| *x.unwrap()).tycx.items;
            let name = match am.find(&id) {
                Some(m) => match m {
                    &ast_map::node_item(ref it, _) => Some(it.ident),
                    _ => fail!("mod id mapped to non-item in the ast map")
                },
                None => None
            };
            let mut om = Module::new(name);
            om.view_items = m.view_items.clone();
            om.where = span;
            om.attrs = attrs;
            for i in m.items.iter() {
                visit_item(*i, &mut om);
            }
            om
        }

        fn visit_item(item: &ast::item, om: &mut Module) {
            debug!("Visiting item %?", item);
            match item.node {
                ast::item_mod(ref m) => {
                    om.mods.push(visit_mod_contents(m, item.attrs.clone(), item.span, item.id));
                },
                ast::item_enum(ref ed, ref gen) => om.enums.push(visit_enum_def(item, ed, gen)),
                ast::item_struct(sd, ref gen) => om.structs.push(visit_struct_def(item, sd, gen)),
                ast::item_fn(ref fd, ref pur, ref abi, ref gen, _) =>
                    om.fns.push(visit_fn(item, fd, pur, abi, gen)),
                ast::item_ty(ref ty, ref gen) => {
                    let t = Typedef {
                        ty: ty.clone(),
                        gen: gen.clone(),
                        name: item.ident,
                        id: item.id,
                        attrs: item.attrs.clone(),
                        where: item.span,
                    };
                    om.typedefs.push(t);
                },
                ast::item_static(ref ty, ref mut_, ref exp) => {
                    let s = Static {
                        type_: ty.clone(),
                        mutability: mut_.clone(),
                        expr: exp.clone(),
                        name: item.ident,
                        attrs: item.attrs.clone(),
                        where: item.span,
                    };
                    om.statics.push(s);
                },
                ast::item_trait(ref gen, ref tr, ref met) => {
                    let t = Trait {
                        name: item.ident,
                        methods: met.clone(),
                        generics: gen.clone(),
                        parents: tr.clone(),
                        id: item.id,
                        attrs: item.attrs.clone(),
                        where: item.span
                    };
                    om.traits.push(t);
                },
                ast::item_impl(ref gen, ref tr, ref ty, ref meths) => {
                    let i = Impl {
                        generics: gen.clone(),
                        trait_: tr.clone(),
                        for_: ty.clone(),
                        methods: meths.clone(),
                        attrs: item.attrs.clone(),
                        where: item.span,
                    };
                    om.impls.push(i);
                },
                _ => (),
            }
        }

        visit_mod(&crate.module, crate.attrs.clone(), crate.span, ast::CRATE_NODE_ID, self);
    }
}
