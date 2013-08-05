use extra::treemap::TreeMap;
use extra::json::{ToJson, Json, Object, String};

use syntax::ast;

use doctree;
use clean;

impl ToJson for clean::Crate {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"schema", String(~"0.5.0"));
        o.insert(~"name", self.name.to_json());
        o.insert(~"mods", self.mods.to_json());
        o.insert(~"attrs", self.attrs.to_json());
        Object(o)
    }
}

impl ToJson for clean::Module {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"name", self.name.to_json());
        o.insert(~"mods", self.mods.to_json());
        o.insert(~"structs", self.structs.to_json());
        o.insert(~"fns", self.fns.to_json());
        o.insert(~"enums", self.enums.to_json());
        o.insert(~"attrs", self.attrs.to_json());
        o.insert(~"typedefs", self.typedefs.to_json());
        o.insert(~"statics", self.statics.to_json());
        Object(o)
    }
}

impl ToJson for clean::Attribute {
    pub fn to_json(&self) -> Json {
        match *self {
            clean::Word(ref s) => String(s.clone()),
            clean::List(ref s, ref l) => {
                let mut o = ~TreeMap::new();
                o.insert(s.clone(), l.to_json());
                Object(o)
            },
            clean::NameValue(ref k, ref v) => {
                let mut o = ~TreeMap::new();
                o.insert(k.clone(), String(v.clone()));
                Object(o)
            }
        }
    }
}

impl ToJson for clean::Struct {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"id", self.node.to_json());
        o.insert(~"name", String(self.name.clone()));
        o.insert(~"type", self.struct_type.to_json());
        o.insert(~"attrs", self.attrs.to_json());
        o.insert(~"fields", self.fields.to_json());
        o.insert(~"generics", self.generics.to_json());
        o.insert(~"source", self.where.to_json());
        Object(o)
    }
}

impl ToJson for doctree::StructType {
    pub fn to_json(&self) -> Json {
        String(self.to_str())
    }
}

impl ToJson for clean::StructField {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
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
        use extra;
        use super::clean::*;
        let mut o = ~TreeMap::new();
        let (n, v) = match self {
            &Unresolved(*) => fail!("no unresolved types should survive to jsonification"),
            &Resolved(n) => (~"resolved", n.to_json()),
            &External(ref p, ref t) => (~"external", {
                let mut o = ~TreeMap::new();
                o.insert(~"path", p.to_json());
                o.insert(~"type", t.to_json());
                Object(o)
            }),
            &Generic(n) => (~"generic", n.to_json()),
            &Self(n) => (~"self", n.to_json()),
            &Primitive(p) => (~"primitive", match p {
                              ast::ty_int(t) => t.to_str().to_json(),
                              ast::ty_uint(t) => t.to_str().to_json(),
                              ast::ty_float(t) => t.to_str().to_json(),
                              _ => fail!("non-numeric primitive survived to jsonification"),
                              }
                             ),
            &Closure(ref c) => (~"closure", c.to_json()),
            &BareFunction(ref b) => (~"barefn", b.to_json()),
            &Unique(ref t) => (~"unique", t.to_json()),
            &Managed(ref t) => (~"managed", t.to_json()),
            &RawPointer(ref t) => (~"unsafe_pointer", t.to_json()),
            &BorrowedRef(ref t) => (~"borrowed", t.to_json()),
            &Tuple(ref t) => (~"tuple", t.to_json()),
            &Vector(ref t) => (~"vector", t.to_json()),
            &String => (~"string", extra::json::String(~"")),
            &Bool => (~"primitive", extra::json::String(~"bool")),
            &Bottom => (~"bottom", extra::json::String(~"")),
            &Unit => (~"unit", extra::json::String(~"")),
        };
        o.insert(~"type", extra::json::String(n));
        if v != extra::json::String(~"") {
            o.insert(~"value", v);
        }
        Object(o)
    }
}

impl ToJson for clean::Lifetime {
    pub fn to_json(&self) -> Json {
        (**self).to_json()
    }
}

impl ToJson for clean::TyParam {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"name", self.name.to_json());
        o.insert(~"bounds", self.bounds.to_json());
        o.insert(~"id", self.node.to_json());
        Object(o)
    }
}

impl ToJson for clean::TyParamBound {
    pub fn to_json(&self) -> Json {
        let ret: Json = match self {
            &clean::RegionBound => String(~"region_bound"),
            &clean::TraitBound(ref t) => {
                let mut o = ~TreeMap::new();
                o.insert(~"trait_bound", t.to_json());
                Object(o)
            }
        };
        ret
    }
}

impl ToJson for clean::Trait {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"name", self.name.to_json());
        o.insert(~"methods", self.methods.to_json());
        o.insert(~"lifetimes", self.lifetimes.to_json());
        o.insert(~"generics", self.generics.to_json());
        Object(o)
    }
}

impl ToJson for clean::Method { //TODO method is a stub right now
    pub fn to_json(&self) -> Json {
        String(~"method placeholder")
    }
}

impl ToJson for clean::Generics {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"lifetimes", self.lifetimes.to_json());
        o.insert(~"typarams", self.type_params.to_json());
        Object(o)
    }
}

impl ToJson for clean::Enum {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"variants", self.variants.to_json());
        o.insert(~"generics", self.generics.to_json());
        o.insert(~"attrs", self.attrs.to_json());
        o.insert(~"name", self.name.to_json());
        o.insert(~"id", self.node.to_json());
        o.insert(~"source", self.where.to_json());
        Object(o)
    }
}

impl ToJson for clean::Variant {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"name", String(self.name.clone()));
        o.insert(~"attrs", self.attrs.to_json());
        o.insert(~"visibility", String(match self.visibility {
            ast::public => ~"public",
            ast::private => ~"private",
            ast::inherited => ~"inherited"
        }));
        o.insert(~"kind", self.kind.to_json());
        Object(o)
    }
}

impl ToJson for clean::VariantStruct {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"type", self.struct_type.to_json());
        o.insert(~"fields", self.fields.to_json());
        Object(o)
    }
}

impl ToJson for clean::VariantKind {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        match self {
            &clean::CLikeVariant => { o.insert(~"type", String(~"c-like")); },
            &clean::TupleVariant(ref args) => {
                o.insert(~"type", String(~"tuple"));
                o.insert(~"members", args.to_json());
            },
            &clean::StructVariant(ref struct_) => {
                o.insert(~"type", String(~"struct"));
                o.insert(~"members", struct_.to_json());
            }
        }
        Object(o)
    }
}

impl ToJson for clean::Function {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"id", self.id.to_json());
        o.insert(~"attrs", self.attrs.to_json());
        o.insert(~"decl", self.decl.to_json());
        o.insert(~"source", self.where.to_json());
        o.insert(~"generics", self.generics.to_json());
        o.insert(~"name", self.name.to_json());
        Object(o)
    }
}

impl ToJson for clean::ClosureDecl {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"sigil", String(~""));
        o.insert(~"region", self.region.to_json());
        o.insert(~"lifetimes", self.lifetimes.to_json());
        o.insert(~"decl", self.decl.to_json());
        o.insert(~"onceness", self.onceness.to_str().to_json());
        o.insert(~"purity", self.purity.to_str().to_json());
        o.insert(~"bounds", self.bounds.to_json());
        Object(o)
    }
}

impl ToJson for clean::FnDecl {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"arguments", self.inputs.to_json());
        o.insert(~"output", self.output.to_json());
        o.insert(~"return_style", self.cf.to_json());
        Object(o)
    }
}

impl ToJson for clean::Argument {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"type", self.ty.to_json());
        o.insert(~"id", self.id.to_json());
        o.insert(~"name", self.name.to_json());
        Object(o)
    }
}

impl ToJson for clean::RetStyle {
    pub fn to_json(&self) -> Json {
        String(match *self {
            clean::NoReturn => ~"no_return",
            clean::Return => ~"return"
        })
    }
}

impl ToJson for clean::Typedef {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"name", self.name.to_json());
        o.insert(~"type", self.type_.to_json());
        o.insert(~"generics", self.generics.to_json());
        o.insert(~"id", self.id.to_json());
        o.insert(~"attrs", self.attrs.to_json());
        o.insert(~"source", self.where.to_json());
        Object(o)
    }
}

impl ToJson for clean::BareFunctionDecl {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"purity", self.purity.to_str().to_json());
        o.insert(~"generics", self.generics.to_json());
        o.insert(~"decl", self.decl.to_json());
        o.insert(~"abi", self.abi.to_json());
        Object(o)
    }
}

impl ToJson for clean::Static {
    pub fn to_json(&self) -> Json {
        let mut o = ~TreeMap::new();
        o.insert(~"name", self.name.to_json());
        o.insert(~"type", self.type_.to_json());
        o.insert(~"mutability", self.mutability.to_str().to_json());
        o.insert(~"expr", self.expr.to_json());
        o.insert(~"attrs", self.attrs.to_json());
        Object(o)
    }
}
