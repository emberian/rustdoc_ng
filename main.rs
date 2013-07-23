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

use rustc::{front, metadata, driver, middle};

use syntax::parse;
use syntax::ast;
use syntax::ast_map;

use std::os;
use std::local_data;
use extra::json::ToJson;

use syntax::visit::Visitor;

use visit::RustdocVisitor;
use clean::Clean;

pub mod doctree;
pub mod clean;
pub mod jsonify;
pub mod visit;

pub static ctxtkey: local_data::Key<@DocContext> = &local_data::Key;

struct DocContext {
    crate: @ast::Crate,
    tycx: middle::ty::ctxt,
    sess: driver::session::Session
}

/// Parses, resolves, and typechecks the given crate
fn get_ast_and_resolve(cpath: &Path, libs: ~[Path]) -> DocContext {
    let parsesess = parse::new_parse_sess(None);
    let sessopts = @driver::session::options {
        binary: @"rustdoc",
        maybe_sysroot: Some(@std::os::self_exe_path().get().pop()),
        addl_lib_search_paths: @mut libs,
        .. (*rustc::driver::session::basic_options()).clone()
    };


    let diagnostic_handler = syntax::diagnostic::mk_handler(None);
    let span_diagnostic_handler =
        syntax::diagnostic::mk_span_handler(diagnostic_handler, parsesess.cm);

    let mut sess = driver::driver::build_session_(sessopts, parsesess.cm,
                                                  syntax::diagnostic::emit,
                                                  span_diagnostic_handler);

    let (crate, tycx) = driver::driver::compile_upto(sess, sessopts.cfg.clone(),
                                                     &driver::driver::file_input(cpath.clone()),
                                                     driver::driver::cu_typeck, None);
                                                     
    DocContext { crate: crate.unwrap(), tycx: tycx.unwrap(), sess: sess }
}

fn main() {
    use extra::getopts::*;
    use std::hashmap::HashMap;

    let args = os::args();
    let opts = ~[
        optmulti("L")
    ];
    let matches = getopts(args.tail(), opts).get();
    let libs = opt_strs(&matches, "L").map(|s| Path(*s));

    let ctxt = @get_ast_and_resolve(&Path(matches.free[0]), libs);
    debug!("defmap:");
    for ctxt.tycx.def_map.iter().advance |(k, v)| {
        debug!("%?: %?", k, v);
    }
    local_data::set(ctxtkey, ctxt);

    let mut v = @mut RustdocVisitor::new();
    v.visit(ctxt.crate);

    let mut crate = v.clean();
    println(crate.to_json().to_str());
}
