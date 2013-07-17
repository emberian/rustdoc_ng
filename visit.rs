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
            let mut struct_type = Plain;
            let mut fields: ~[StructField] = vec::with_capacity(sd.fields.len());
            if sd.ctor_id.is_some() {
                // We are in a unit/tuple struct
                match sd.fields.len() {
                    0 => struct_type = Unit,
                    1 => struct_type = Newtype,
                    _ => struct_type = Tuple
                }
            }

            for sd.fields.iter().advance |x| {
                fields.push(StructField {
                            id: x.node.id,
                            type_:  copy x.node.ty,
                            attrs:  copy x.node.attrs,
                            name:  match x.node.kind { ast::named_field(id, _) => Some(id), _ => None },
                            visibility: match x.node.kind {
                                ast::named_field(_, vis) => Some(vis),
                                _ => None
                            },
                            });
            }
            rcx.structs.push(
                Struct {
                    id: item.id,
                    struct_type: struct_type,
                    name: item.ident,
                    attrs: copy item.attrs,
                    generics: copy *generics,
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
                    attrs: copy x.node.attrs,
                    visibility: x.node.vis
                });
            }
            rcx.enums.push(Enum {
                name: it.ident,
                variants: vars,
                generics: copy *params,
                attrs: copy it.attrs,
                id: it.id,
                where: it.span,
            });
        }

        fn visit_fn(item: &ast::item, fd: &ast::fn_decl, purity: &ast::purity,
                     abi: &AbiSet, gen: &ast::Generics, rcx: rdv) {
            debug!("Visiting fn");
            rcx.fns.push(Function {
                id: item.id,
                attrs: copy item.attrs,
                decl: copy *fd,
                name: item.ident,
                visibility: item.vis,
                where: item.span,
                generics: copy *gen,
            });
        }

        fn visit_item(item: @ast::item, (rcx, vt): (rdv, vst)) {
            debug!("Visiting item %?", item);
            match item.node {
                ast::item_mod(ref m) => {
                    for m.items.iter().advance |i| { (vt.visit_item)(*i, (copy rcx, vt)); }
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
