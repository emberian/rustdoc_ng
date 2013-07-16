//! AST Visitor. Extracts useful information and massages it into a form usable for rustdoc

use std::vec;

use syntax::visit_new::{Visitor,FnKind};
use syntax::ast;
use syntax::codemap::span;

use doctree::*;

pub struct RustdocVisitor {
    structs: ~[Struct],
    enums: ~[Enum],
    funcs: ~[Function]
}

impl RustdocVisitor {
    pub fn new() -> RustdocVisitor {
        RustdocVisitor {
            structs: ~[],
            enums: ~[],
            funcs: ~[]
        }
    }
}

impl Visitor for RustdocVisitor {
    pub fn visit_struct_def(&mut self, sd: @ast::struct_def, nm: ast::ident, generics:
                             &ast::Generics, id: ast::node_id) {
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

        for sd.fields.iter().advance |&x| {
            fields.push(StructField {
                        id: x.node.id,
                        type_:  x.node.ty,
                        attrs:  copy x.node.attrs,
                        name:  match x.node.kind { ast::named_field(id, _) => Some(id), _ => None },
                        visibility: match x.node.kind {
                            ast::named_field(_, vis) => Some(vis),
                            _ => None
                        },
                        });
        }
        self.structs.push(
            Struct {
                node: id,
                struct_type: struct_type,
                name: nm,
                attrs: ~[],
                generics: copy *generics,
                fields: fields
            }
        );
    }

    pub fn visit_enum_def(&mut self, def: &ast::enum_def, params: &ast::Generics) {
        let mut vars: ~[Variant] = ~[];
        for def.variants.iter().advance |&x| {
            vars.push(Variant {
                name: x.node.name,
                attrs: copy x.node.attrs,
                id: x.node.id,
                visibility: x.node.vis
            });
        }
        self.enums.push(Enum {
            variants: vars,
            generics: copy *params,
            attrs: ~[]
        });
    }

    pub fn visit_fn(&mut self, fk: &FnKind, fd: &ast::fn_decl, body: &ast::blk, sp: span, id: ast::node_id) {
        self.funcs.push(Function {
            id: id,
            decl: copy *fd,
            body: copy *body
        });
    }
}
