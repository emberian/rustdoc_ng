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
        o.insert(~"type", String(self.type_.to_str()));
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

