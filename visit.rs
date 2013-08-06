use clean;
use clean::Item;

trait DocVisitor {
    pub fn visit_item(&mut self, &mut Item);
    pub fn visit_mod(&mut self, m: &mut clean::Module) {
        for x in m.structs.mut_iter() {
            self.visit_item(x);
        }
        for x in m.enums.mut_iter() {
            self.visit_item(x);
        }
        for x in m.fns.mut_iter() {
            self.visit_item(x);
        }
        for x in m.mods.mut_iter() {
            self.visit_item(x);
            self.visit_mod(match x.inner {
                               clean::ModuleItem(ref mut m) => m,
                               _ => fail!("non-module in ModuleItem")
                           });
        }
        for x in m.typedefs.mut_iter() {
            self.visit_item(x);
        }
        for x in m.statics.mut_iter() {
            self.visit_item(x);
        }
        for x in m.traits.mut_iter() {
            self.visit_item(x);
        }
        for x in m.impls.mut_iter() {
            self.visit_item(x);
        }
        for x in m.view_items.mut_iter() {
            self.visit_item(x);
        }
    }
    pub fn visit_crate(&mut self, &mut clean::Crate);
}
