//! AST Visitor. Extracts useful information and massages it into a form usable for rustdoc

use std::vec;

use syntax::abi::AbiSet;
use syntax::{visit, ast, ast_map};
use syntax::visit::{Visitor};
use syntax::codemap::span;

use doctree::*;
use std::local_data;

pub struct RustdocVisitor {
    mods: ~[Module],
    attrs: ~[ast::Attribute],
}

impl RustdocVisitor {
    pub fn new() -> RustdocVisitor {
        RustdocVisitor {
            mods: ~[],
            attrs: ~[],
        }
    }
}

type rdv = @mut RustdocVisitor;
type vst = visit::vt<rdv>;

impl RustdocVisitor {
    pub fn visit(@mut self, crate: &ast::Crate) {
        self.attrs = crate.attrs.clone();
        fn visit_struct_def(item: &ast::item, sd: @ast::struct_def, generics:
                            &ast::Generics) -> Struct {
            debug!("Visiting struct");
            let struct_type = struct_type_from_def(sd);
            let mut fields: ~[StructField] = vec::with_capacity(sd.fields.len());

            for sd.fields.iter().advance |x| {
                fields.push(StructField::new(&x.node));
            }

            Struct {
                id: item.id,
                struct_type: struct_type,
                name: item.ident,
                attrs: item.attrs.clone(),
                generics: generics.clone(),
                fields: fields,
                where: item.span
            }
        }

        fn visit_enum_def(it: &ast::item, def: &ast::enum_def, params: &ast::Generics) -> Enum {
            debug!("Visiting enum");
            let mut vars: ~[Variant] = ~[];
            for def.variants.iter().advance |x| {
                vars.push(Variant {
                    name: x.node.name,
                    attrs: x.node.attrs.clone(),
                    visibility: x.node.vis,
                    kind: x.node.kind.clone(),
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
        fn visit_mod(m: &ast::_mod, span: span, id: ast::NodeId, (rcx, _vt): (rdv, vst)) {
            rcx.mods.push(visit_mod_contents(m, span, id));
        }

        fn visit_mod_contents(m: &ast::_mod, _span: span, id: ast::NodeId) -> Module {
            let am = local_data::get(super::ctxtkey, |x| *x.unwrap()).tycx.items;
            let name = match am.find(&id) {
                Some(m) => match m {
                    &ast_map::node_item(ref it, _) => Some(it.ident),
                    _ => fail!("mod id mapped to non-item in the ast map")
                },
                None => None
            };
            let mut om = Module::new(name);
            for m.items.iter().advance |i| {
                visit_item(*i, &mut om);
            }
            om
        }

        fn visit_item(item: &ast::item, om: &mut Module) {
            debug!("Visiting item %?", item);
            match item.node {
                ast::item_mod(ref m) => {
                    om.mods.push(visit_mod_contents(m, item.span, item.id));
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
                    };
                    om.statics.push(s);
                },
                _ => (),
            }
        }

        let visitor = Visitor {
            visit_mod: visit_mod,
            .. *visit::default_visitor::<@mut RustdocVisitor>()
        };

        visit::visit_crate(crate, (self, visit::mk_vt(@visitor)));
    }
}
