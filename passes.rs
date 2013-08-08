use clean;
use clean::Item;
use plugins;
use fold;
use fold::DocFolder;

/// A sample pass showing the minimum required work for a plugin.
pub fn noop(crate: clean::Crate) -> plugins::PluginResult {
    (crate, None)
}

pub fn strip_hidden(crate: clean::Crate) -> plugins::PluginResult {
    struct Stripper;
    impl fold::DocFolder for Stripper {
        pub fn fold_item(&mut self, i: Item) -> Option<Item> {
            for attr in i.attrs.iter() {
                match attr {
                    &clean::List(~"doc", ref l) => {
                        for innerattr in l.iter() {
                            match innerattr {
                                &clean::Word(ref s) if "hidden" == *s => {
                                    info!("found one in strip_hidden; returning false");
                                    return None;
                                },
                                _ => (),
                            }
                        }
                    },
                    _ => ()
                }
            }
            self.fold_item_recur(i)
        }
    }
    let mut stripper = Stripper;
    let crate = stripper.fold_crate(crate);
    (crate, None)
}

pub fn clean_comments(crate: clean::Crate) -> plugins::PluginResult {
    (crate, None)
}
