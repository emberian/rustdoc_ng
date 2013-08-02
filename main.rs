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

use extra::json::ToJson;

pub mod core;
pub mod doctree;
pub mod clean;
pub mod jsonify;
pub mod visit;
pub mod plugins;

pub static ctxtkey: std::local_data::Key<@core::DocContext> = &std::local_data::Key;


fn main() {
    use extra::getopts::*;

    let args = std::os::args();
    let opts = ~[
        optmulti("L"),
        optmulti("p"),
    ];

    let matches = getopts(args.tail(), opts).get();
    let libs = opt_strs(&matches, "L").map(|s| Path(*s));

    let cratepath = Path(matches.free[0]);

    let mut crate = core::run_core(libs, &cratepath);
    let mut json = match crate.to_json() {
        extra::json::Object(o) => o,
        _ => fail!("JSON returned was not an object")
    };

    let mut pm = plugins::PluginManager::new(Path("/tmp/rustdoc_ng/plugins"));

    for opt_strs(&matches, "p").consume_iter().advance |pname| {
        pm.load_plugin(pname);
    }

    let res = pm.run_plugins(&mut crate);
    for res.iter().advance |&(ref s, ref toj)| {
        json.insert(s.clone(), toj.to_json());
    }

    println(extra::json::Object(json).to_str());
}
