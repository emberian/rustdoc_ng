#[link(name = "rustdoc_ng",
       vers = "0.1.0",
       uuid = "8c6e4598-1596-4aa5-a24c-b811914bbbc6")];
#[desc = "rustdoc, the Rust documentation extractor"];
#[license = "MIT/ASL2"];
#[crate_type = "bin"];

//#[deny(warnings)];

extern mod syntax;
extern mod rustc;

extern mod extra;

use std::cell::Cell;
use extra::json::ToJson;

pub mod core;
pub mod doctree;
pub mod clean;
pub mod jsonify;
pub mod visit;
pub mod plugins;
mod passes;

pub static ctxtkey: std::local_data::Key<@core::DocContext> = &std::local_data::Key;


fn main() {
    use extra::getopts::*;
    use extra::getopts::groups::*;

    let args = std::os::args();
    let opts = ~[
        optmulti("L", "library-path", "directory to add to crate search path", "DIR"),
        optmulti("p", "plugin", "plugin to load and run", "NAME"),
        optmulti("", "plugin-path", "directory to load plugins from", "DIR"),
        // auxillary pass (defaults to hidden_strip
        optmulti("a", "pass", "auxillary pass to run", "NAME"),
        optflag("n", "no-defult-passes", "do not run the default passes"),
        optflag("h", "help", "show this help message"),
    ];

    let matches = getopts(args.tail(), opts).get();

    if opt_present(&matches, "h") || opt_present(&matches, "help") {
        println(usage(args[0], opts));
        return;
    }

    let libs = Cell::new(opt_strs(&matches, "L").map(|s| Path(*s)));

    let mut passes = if opt_present(&matches, "n") {
        ~[]
    } else {
        ~[~"strip-hidden", ~"clean-comments"]
    };

    opt_strs(&matches, "a").map(|x| passes.push(x.clone()));

    if matches.free.len() != 1 {
        println(usage(args[0], opts));
        return;
    }

    let cr = Cell::new(Path(matches.free[0]));

    let mut crate = std::task::try(|| {let cr = cr.take(); core::run_core(libs.take(), &cr)}).unwrap();
    let mut json = match crate.to_json() {
        extra::json::Object(o) => o,
        _ => fail!("JSON returned was not an object")
    };

    let mut pm = plugins::PluginManager::new(Path("/tmp/rustdoc_ng/plugins"));

    foreach pass in passes.iter() {
        pm.add_plugin(match pass.as_slice() {
            "strip-hidden" => passes::strip_hidden,
            "clean-comments" => passes::clean_comments,
            s => { error!("unknown pass %s", s); passes::noop },
        })
    }

    for pname in opt_strs(&matches, "p").consume_iter() {
        pm.load_plugin(pname);
    }

    let res = pm.run_plugins(&mut crate);
    foreach result in res.iter() {
        match result {
            &Some((ref s, ref toj)) => { json.insert(s.clone(), toj.to_json()); },
            &None => (),
        }
    }

    println(extra::json::Object(json).to_str());
}
