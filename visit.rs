use clean;

pub fn visit_each_item_mod<T>(m: &mut clean::Module, f: &fn(&mut Item<T>)) {
    for x in m.structs.mut_iter() {
        f(x)
    }
    for x in m.enums.mut_iter() {
        f(x)
    }
    for x in m.fns.mut_iter() {
        f(x)
    }
    for x in m.mods.mut_iter() {
        visit_each_item_mod(x, f(x))
    }
    for x in m.typedefs.mut_iter() {
        f(x)
    }
    for x in m.statics.mut_iter() {
        f(x)
    }
    for x in m.traits.mut_iter() {
        f(x)
    }
    for x in m.impls.mut_iter() {
        f(x)
    }
    for x in m.view_items.mut_iter() {
        f(x)
    }
}
