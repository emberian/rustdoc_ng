use clean;
use clean::Item;
use plugins;
use visit;

pub fn noop(_crate: &mut clean::Crate) -> plugins::PluginResult {
    None
}

pub fn strip_hidden(_crate: &mut clean::Crate) -> plugins::PluginResult {
    struct Stripper;
    impl visit::DocVisitor for Stripper {
        pub fn visit_item_contents(&mut self, i: &mut Item) -> bool {
            for attr in i.attrs.iter() {
                match attr {
                    clean::List(~"doc", ref l) => {
                        for innerattr in l.iter() {
                            match innerattr {
                                clean::Word(ref s) => {
                                    if ~"hidden" == s {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn clean_comments(_crate: &mut clean::Crate) -> plugins::PluginResult {
    None
}
