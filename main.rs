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
    let mut crate = core::run_core(std::os::args());
    println(crate.to_json().to_str());
}
