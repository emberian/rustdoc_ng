//! AST Visitor. Extracts useful information and massages it into a form usable for rustdoc

use std::vec;

use syntax;
use syntax::abi::AbiSet;
use syntax::visit;
use syntax::visit::{Visitor, fn_kind};
use syntax::ast;
use syntax::codemap::span;

use doctree::*;
use std::local_data;

pub struct RustdocVisitor {
    structs: ~[Struct],
    enums: ~[Enum],
    fns: ~[Function]
}

impl RustdocVisitor {
    pub fn new() -> RustdocVisitor {
        RustdocVisitor {
            structs: ~[],
            enums: ~[],
            fns: ~[]
        }
    }
}

type rdv = @mut RustdocVisitor;
type vst = visit::vt<rdv>;

impl RustdocVisitor {
    pub fn visit(@mut self, crate: &ast::crate) {
        fn visit_struct_def(item: &ast::item, sd: @ast::struct_def, generics:
                            &ast::Generics, rcx: rdv) {
            debug!("Visiting struct");
            let struct_type = struct_type_from_def(sd);
            let mut fields: ~[StructField] = vec::with_capacity(sd.fields.len());

            for sd.fields.iter().advance |x| {
                fields.push(StructField::new(&x.node));
            }
            rcx.structs.push(
                Struct {
                    id: item.id,
                    struct_type: struct_type,
                    name: item.ident,
                    attrs: item.attrs.clone(),
                    generics: generics.clone(),
                    fields: fields,
                    where: item.span
                }
            );
        }

        fn visit_enum_def(it: &ast::item, def: &ast::enum_def, params: &ast::Generics, rcx: rdv) {
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
            rcx.enums.push(Enum {
                name: it.ident,
                variants: vars,
                generics: params.clone(),
                attrs: it.attrs.clone(),
                id: it.id,
                where: it.span,
            });
        }

        fn visit_fn(item: &ast::item, fd: &ast::fn_decl, purity: &ast::purity,
                     abi: &AbiSet, gen: &ast::Generics, rcx: rdv) {
            debug!("Visiting fn");
            rcx.fns.push(Function {
                id: item.id,
                attrs: item.attrs.clone(),
                decl: fd.clone(),
                name: item.ident,
                visibility: item.vis,
                where: item.span,
                generics: gen.clone(),
            });
        }

        fn visit_item(item: @ast::item, (rcx, vt): (rdv, vst)) {
            debug!("Visiting item %?", item);
            match item.node {
                ast::item_mod(ref m) => {
                    for m.items.iter().advance |i| { (vt.visit_item)(*i, (rcx.clone(), vt)); }
                },
                ast::item_enum(ref ed, ref gen) => visit_enum_def(item, ed, gen, rcx),
                ast::item_struct(sd, ref gen) => visit_struct_def(item, sd, gen, rcx),
                ast::item_fn(ref fd, ref pur, ref abi, ref gen, _) =>
                    visit_fn(item, fd, pur, abi, gen, rcx),
                _ => (),
            }
        }

        let visitor = Visitor {
            visit_item: visit_item,
            .. *visit::default_visitor::<@mut RustdocVisitor>()
        };

        visit::visit_crate(crate, (self, visit::mk_vt(@visitor)));
    }
}
