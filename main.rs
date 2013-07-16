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

pub static ctxtkey: local_data::Key<DocContext> = &local_data::Key;

struct DocContext {
    crate: @ast::crate,
    cmap: middle::resolve::CrateMap,
    amap: ast_map::map,
    sess: driver::session::Session
}

fn get_ast_and_resolve(cpath: &Path, libs: ~[Path]) -> DocContext {
    let parsesess = parse::new_parse_sess(None);
    let sessopts = @driver::session::options {
        binary: @"rustdoc",
        maybe_sysroot: Some(@std::os::self_exe_path().get().pop()),
        addl_lib_search_paths: @mut libs,
        ..copy *rustc::driver::session::basic_options()
    };


    let mut crate = parse::parse_crate_from_file(cpath, ~[], parsesess);
    // XXX: these need to be kept in sync with the pass order in rustc::driver::compile_rest
    let diagnostic_handler = syntax::diagnostic::mk_handler(None);
    let span_diagnostic_handler =
        syntax::diagnostic::mk_span_handler(diagnostic_handler, parsesess.cm);

    let mut sess = driver::driver::build_session_(sessopts, parsesess.cm,
                                                  syntax::diagnostic::emit,
                                                  span_diagnostic_handler);

    crate = front::config::strip_unconfigured_items(crate);
    crate = syntax::ext::expand::expand_crate(sess.parse_sess, ~[], crate);
    crate = front::config::strip_unconfigured_items(crate);
    crate = front::std_inject::maybe_inject_libstd_ref(sess, crate);

    let ast_map = syntax::ast_map::map_crate(sess.diagnostic(), crate);
    let meta_os = driver::session::sess_os_to_meta_os(sess.targ_cfg.os);
    let id_int = parse::token::get_ident_interner();
    metadata::creader::read_crates(sess.diagnostic(), crate, sess.cstore, sess.filesearch, meta_os,
                                   sess.opts.is_static, id_int);
    let lang_items = middle::lang_items::collect_language_items(crate, sess);
    let cmap = middle::resolve::resolve_crate(sess, lang_items, crate);
    middle::entry::find_entry_point(sess, crate, ast_map);
    let freevars = middle::freevars::annotate_freevars(cmap.def_map, crate);
    let region_map = middle::region::resolve_crate(sess, cmap.def_map, crate);
    middle::entry::find_entry_point(sess, crate, ast_map);
    let freevars = middle::freevars::annotate_freevars(cmap.def_map, crate);
    let region_map = middle::region::resolve_crate(sess, cmap.def_map, crate);
    let rp_set = middle::region::determine_rp_in_crate(sess, ast_map, cmap.def_map, crate);
    let ty_cx = middle::ty::mk_ctxt(sess, cmap.def_map, ast_map, freevars, region_map, rp_set, lang_items);

    let (_mmap, _vmap) = middle::typeck::check_crate(ty_cx, copy cmap.trait_map, crate);
    DocContext { crate: crate, cmap: cmap, amap: ast_map, sess: sess }
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

    let ctxt = get_ast_and_resolve(&Path(matches.free[0]), libs);
    debug!("defmap:");
    for ctxt.cmap.def_map.iter().advance |(k, v)| {
        debug!("%?: %?", k, v);
    }
    local_data::set(ctxtkey, ctxt);

    let mut v = RustdocVisitor::new();
    v.visit_crate(ctxt.crate);
    let mut crate = v.clean();

    for crate.fns.mut_iter().advance |x| {
        x.attrs = match ctxt.amap.get(&x.id) {
            &ast_map::node_item(item, _path) => item.attrs.iter().transform(|x| x.clean()).collect(),
            _ => fail!("function mapped to non-item")
        }
    }
    println(crate.to_json().to_str());
}
