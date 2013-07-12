//! AST Visitor. Extracts useful information and massages it into a form usable for rustdoc

use std::vec;

use syntax::visit_new::Visitor;
use syntax::ast;

use doctree::*;

pub struct RustdocVisitor {
    structs: ~[Struct]
}

impl RustdocVisitor {
    pub fn new() -> RustdocVisitor {
        RustdocVisitor {
            structs: ~[]
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
}
