use std;
use clean;
use clean::Item;
use std::iterator::Extendable;

pub trait DocVisitor {
    /// Visit every item in a vector, doing filtering like visit_item
    /// describes
    pub fn visit_item_vec(&mut self, field: &mut ~[Item]) {
        use std::util::swap;
        let mut foo = ~[]; swap(field, &mut foo);
        field.extend(&mut foo.consume_iter().filter_map(|x| self.visit_item(x)));
    }
    /// Visit an item, returning Some(item) to replace the item in the AST,
    /// and None to remove it. Note that just returning the Item you're given
    /// is equivalent to not changing it.
    pub fn visit_item(&mut self, mut item: Item) -> Option<Item> {
        use clean::*;

        let r = self.visit_item_contents(&mut item);
        if !r {
            return None;
        }

        match item.inner {
            StructItem(ref mut i) => {
                self.visit_item_vec(&mut i.fields);
            },
            ModuleItem(ref mut i) => {
                self.visit_mod(i);
            },
            EnumItem(ref mut i) => {
                self.visit_item_vec(&mut i.variants);
            },
            TraitItem(ref mut i) => {
                fn vtrm<T: DocVisitor>(this: &mut T, trm: TraitMethod) -> Option<TraitMethod> {
                    match trm {
                        Required(it) => {
                            match this.visit_item(it) {
                                Some(x) => return Some(Required(x)),
                                None => return None,
                            }
                        },
                        Provided(it) => {
                            match this.visit_item(it) {
                                Some(x) => return Some(Provided(x)),
                                None => return None,
                            }
                        },
                    }
                }
                let mut foo = ~[]; std::util::swap(&mut i.methods, &mut foo);
                i.methods.extend(&mut foo.consume_iter().filter_map(|x| vtrm(self, x)));
            },
            ImplItem(ref mut i) => {
                self.visit_item_vec(&mut i.methods);
            },
            VariantItem(ref mut i) => {
                match i.kind {
                    StructVariant(ref mut j) => {
                        self.visit_item_vec(&mut j.fields);
                    },
                    _ => (),
                };
            },
            _ => (),
        };
        Some(item)
    }
    /// Visit an item's contents. This is the default "user-overridable"
    /// method. Return true to keep the item in the AST, and false to remove
    /// it, much like visit_item, but with a mutable reference and not a
    /// value.
    pub fn visit_item_contents(&mut self, &mut Item) -> bool;
    /// Visit every item in a module. For visiting submodules, it first visits
    /// the item and drops it if visit_item returns false, and then calls
    /// visit_mod on it (thus achieving recursion).
    pub fn visit_mod(&mut self, m: &mut clean::Module) {
        use std::util::swap;

        let fields = ~[&mut m.structs, &mut m.enums, &mut m.fns, &mut m.typedefs,
                       &mut m.statics, &mut m.traits, &mut m.impls, &mut m.view_items];

        for field in fields.consume_iter() { 
            self.visit_item_vec(field);
        }

        let mut foo = ~[]; swap(&mut m.mods, &mut foo);
        m.mods.extend(&mut foo.consume_iter().filter_map(|x| self.visit_item(x)).transform(|mut x| {
            {let m_ = match x.inner {
                clean::ModuleItem(ref mut m) => m,
                _ => fail!("non-module in ModuleItem")
            };
            info!("Visiting submodule %? (%?)",x.name,  m_);
            self.visit_mod(m_);
            }
            x
            }));
    }
    pub fn visit_crate(&mut self, c: &mut clean::Crate) {
        self.visit_mod(match c.module.inner { clean::ModuleItem(ref mut m) => m, _ => fail!() });
    }
}
