use clean;
use clean::Item;
use std::iterator::Extendable;

trait DocVisitor {
    /// Visit an item, returning Some(item) to replace the item in the AST,
    /// and None to remove it. Note that just returning the Item you're given
    /// is equivalent to not changing it.
    pub fn visit_item(&mut self, mut i: Item) -> Option<Item> {
        let r = self.visit_item_contents(&mut i);
        if r {
            Some(i)
        } else {
            None
        }
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
            let mut foo = ~[]; swap(field, &mut foo);
            field.extend(&mut foo.consume_iter().filter_map(|mut x| self.visit_item(x)));
        }

        let mut foo = ~[]; swap(&mut m.mods, &mut foo);
        m.mods.extend(&mut foo.consume_iter().filter_map(|x| self.visit_item(x)).transform(|x| {
            {let m_ = match x.inner {
                clean::ModuleItem(ref mut m) => m,
                _ => fail!("non-module in ModuleItem")
            };
            self.visit_mod(m_);
            }
            x
            }));
    }
    pub fn visit_crate(&mut self, &mut clean::Crate);
}
