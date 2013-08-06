use clean;
use clean::Item;

enum Action {
    Keep,
    Remove,
}

trait DocVisitor {
    /// Visit an item, potentially mutating it. The return value is true if
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
        m.structs    = m.structs.mut_iter().filter(|x| self.visit_item(*x)).transform(|x| x.clone()).to_owned_vec();
        m.enums      = m.enums.mut_iter().filter(|x| self.visit_item(*x)).transform(|x| x.clone()).to_owned_vec();
        m.fns        = m.fns.mut_iter().filter(|x| self.visit_item(*x)).transform(|x| x.clone()).to_owned_vec();
        m.typedefs   = m.typedefs.mut_iter().filter(|x| self.visit_item(*x)).transform(|x| x.clone()).to_owned_vec();
        m.statics    = m.statics.mut_iter().filter(|x| self.visit_item(*x)).transform(|x| x.clone()).to_owned_vec();
        m.traits     = m.traits.mut_iter().filter(|x| self.visit_item(*x)).transform(|x| x.clone()).to_owned_vec();
        m.impls      = m.impls.mut_iter().filter(|x| self.visit_item(*x)).transform(|x| x.clone()).to_owned_vec();
        m.view_items = m.view_items.mut_iter().filter(|x| self.visit_item(*x)).transform(|x| x.clone()).to_owned_vec();
        for x in m.mods.mut_iter().filter(|x| self.visit_item(*x)) {
            let m_ = match x.inner {
                clean::ModuleItem(ref mut m) => m,
                _ => fail!("non-module in ModuleItem")
            };
            self.visit_mod(m_);
        }
    }
    pub fn visit_crate(&mut self, &mut clean::Crate);
}
