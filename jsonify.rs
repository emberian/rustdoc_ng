use std::hashmap::HashMap;
use extra::json::{ToJson, Json, Object, String};

use syntax::ast;
use clean;

impl ToJson for clean::Attribute {
    pub fn to_json(&self) -> Json {
        match *self {
            clean::Word(ref s) => String(s.clone()),
            clean::List(ref s, ref l) => {
                let mut o = ~HashMap::new();
                o.insert(s.clone(), l.to_json());
                Object(o)
            },
            clean::NameValue(ref k, ref v) => {
                let mut o = ~HashMap::new();
                o.insert(k.clone(), String(v.clone()));
                Object(o)
            }
        }
    }
}

impl ToJson for clean::Struct {
    pub fn to_json(&self) -> Json {
        let mut o = ~HashMap::new();
        o.insert(~"id", String(self.node.to_str()));
        o.insert(~"name", String(self.name.clone()));
        o.insert(~"type", String(self.struct_type.to_str()));
        o.insert(~"attrs", self.attrs.to_json());
        o.insert(~"fields", self.fields.to_json());
        Object(o)
    }
}

impl ToJson for clean::StructField {
    pub fn to_json(&self) -> Json {
        let mut o = ~HashMap::new();
        o.insert(~"name", String(self.name.clone()));
        o.insert(~"type", self.type_.to_json());
        o.insert(~"attrs", self.attrs.to_json());
        o.insert(~"visibility", String(match self.visibility {
                                       None => ~"",
                                       Some(v) => match v {
                                       ast::public    => ~"public",
                                       ast::private   => ~"private",
                                       ast::inherited => ~"inherit"
                                       }
                                       }));
        Object(o)
    }
}

impl ToJson for clean::Type {
    pub fn to_json(&self) -> Json {
        use super::clean::*;
        use extra;
        let mut o = ~HashMap::new();
        let (n, v) = match self {
            &Unresolved(_) => fail!("no unresolved types should survive to jsonification"),
            &Resolved(n) => (~"resolved", extra::json::String(n.to_str())),
            &Primitive(p) => (~"primitive", match p {
                              ast::ty_int(_) => extra::json::String(~"int"),
                              ast::ty_uint(_) => extra::json::String(~"uint"),
                              ast::ty_float(_) => extra::json::String(~"float"),
                              _ => fail!("non-numeric primitive survived to jsonification"),
                              }
                             ),
                             &Tuple(ref t) => (~"tuple", t.to_json()),
                             &Vector(ref t) => (~"vector", t.to_json()),
                             &String => (~"string", extra::json::String(~"")),
                             &Bool => (~"bool", extra::json::String(~"")),
                             &Unit => (~"unit", extra::json::String(~"")),
        };
        o.insert(~"type", extra::json::String(n));
        if v != extra::json::String(~"") {
            o.insert(~"value", v);
        }
        Object(o)
    }
}
