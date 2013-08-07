use clean;
use clean::Item;
use std::iterator::Extendable;

trait DocVisitor {
    /// Visit an item, potentially mutating it. The return value is true
    /// you want the item to be removed from the AST, false otherwise.
    pub fn visit_item(&mut self, i: &mut Item) -> bool {
        self.visit_item_contents(i)
    }
    /// Visit an item's contents. This is the default "user-overridable"
    /// method. The return value is the same as visit_item, and visit_item (by
    /// default) indeed calls visit_item_contents before doing its thing for
    /// any potential child items.
    pub fn visit_item_contents(&mut self, &mut Item) -> bool;
    /// Visit every item in a module. For visiting submodules, it first visits
    /// the item and drops it if visit_item returns false, and then calls
    /// visit_mod on it (thus achieving recursion).
    pub fn visit_mod(&mut self, m: &mut clean::Module) {
        use std::util::swap;

        let mut foo = ~[]; swap(&mut m.structs, &mut foo);
        m.structs.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));
        let mut foo = ~[]; swap(&mut m.enums, &mut foo);
        m.enums.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));
        let mut foo = ~[]; swap(&mut m.fns, &mut foo);
        m.fns.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));
        let mut foo = ~[]; swap(&mut m.mods, &mut foo);
        m.mods.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));
        let mut foo = ~[]; swap(&mut m.typedefs, &mut foo);
        m.typedefs.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));
        let mut foo = ~[]; swap(&mut m.statics, &mut foo);
        m.statics.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));
        let mut foo = ~[]; swap(&mut m.traits, &mut foo);
        m.traits.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));
        let mut foo = ~[]; swap(&mut m.impls, &mut foo);
        m.impls.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));
        let mut foo = ~[]; swap(&mut m.view_items, &mut foo);
        m.view_items.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)));

        let mut foo = ~[]; swap(&mut m.mods, &mut foo);
        m.mods.extend(&mut foo.consume_iter().filter(|x| self.visit_item(x)).transform(|x| {
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
