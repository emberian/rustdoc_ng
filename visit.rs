//! AST Visitor. Extracts useful information and massages it into a form usable for rustdoc

use std::vec;

use syntax;
use syntax::visit_new::Visitor;
use syntax::ast;

use doctree::*;

pub struct RustdocVisitor {
    structs: ~[Struct],
    enums: ~[Enum]
}

impl RustdocVisitor {
    pub fn new() -> RustdocVisitor {
        RustdocVisitor {
            structs: ~[],
            enums: ~[]
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
        let am = unsafe { ::std::local_data::local_data_get(super::ctxtkey).unwrap() }.amap;
        self.structs.push(
            Struct {
                node: id,
                struct_type: struct_type,
                name: nm,
                attrs: match am.get(&id) {
                    &syntax::ast_map::node_item(item, _) => 
                        item.attrs.iter().transform(|x| *x).collect(),
                    _ => fail!("struct's node_id mapped to bogus node in the ast map")
                },
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
            generics: copy *params
        });
    }
}
