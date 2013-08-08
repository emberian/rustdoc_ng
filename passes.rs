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
    struct CommentCleaner;
    impl fold::DocFolder for CommentCleaner {
        pub fn fold_item(&mut self, i: Item) -> Option<Item> {
            let mut i = i;
            let mut avec: ~[clean::Attribute] = ~[];
            for attr in i.attrs.iter() {
                match attr {
                    &clean::NameValue(~"doc", ref s) => avec.push(
                        clean::NameValue(~"doc", clean_comment_body(s.clone()))),
                    x => avec.push(x.clone())
                }
            }
            i.attrs = avec;
            self.fold_item_recur(i)
        }
    }
    let mut cleaner = CommentCleaner;
    let crate = cleaner.fold_crate(crate);
    (crate, None)
}

pub fn collapse_docs(crate: clean::Crate) -> plugins::PluginResult {
    struct Collapser;
    impl fold::DocFolder for Collapser {
        pub fn fold_item(&mut self, i: Item) -> Option<Item> {
            let mut docstr = ~"";
            let mut i = i;
            for attr in i.attrs.iter() {
                match *attr {
                    clean::NameValue(~"doc", ref s) =>
                        docstr.push_str(fmt!("%s\n", s.clone())),
                    _ => ()
                }
            }
            let mut a: ~[clean::Attribute] = i.attrs.iter().filter(|&a| match a {
                &clean::NameValue(~"doc", _) => false,
                _ => true
            }).transform(|x| x.clone()).collect();
            a.push(clean::NameValue(~"doc", docstr.trim().to_owned()));
            i.attrs = a;
            self.fold_item_recur(i)
        }
    }
    let mut collapser = Collapser;
    let crate = collapser.fold_crate(crate);
    (crate, None)
}

//Utility
enum CleanCommentStates {
    Collect,
    Strip,
    Stripped,
}

fn clean_comment_body(s: ~str) -> ~str {
    let mut res = ~"";
    let mut state = Strip;

    for char in s.iter() {
        match (state, char) {
            (Strip, '*') => state = Stripped,
            (Strip, '/') => state = Stripped,
            (Stripped, '/') => state = Strip,
            (Strip, ' ') => (),
            (Strip, '\t') => (),
            (Stripped, ' ') => { res.push_char(char); state = Collect; }
            (Stripped, '\t') => { res.push_char(char); state = Collect; }
            (Strip, _) => { res.push_char(char); state = Stripped; }
            (_, '\n') => { res.push_char(char); state = Strip; }
            (_, char) => res.push_char(char)
        }
    }

    res = res.trim().to_owned();
    res.push_char('\n');
    res
}
