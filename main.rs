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
use extra::json::ToJson;

use visit::*;
use syntax::visit_new::Visitor;

use clean::Clean;

pub mod doctree;
pub mod clean;
mod jsonify;
mod visit;

fn get_ast_and_resolve(cpath: &Path) -> (@ast::crate, middle::resolve::CrateMap, ast_map::map,
                                         middle::typeck::method_map, middle::typeck::vtable_map,
                                         middle::ty::ctxt) {
    let parsesess = parse::new_parse_sess(None);
    let sessopts = @driver::session::options {
        binary: @"rustdoc",
        maybe_sysroot: Some(@std::os::self_exe_path().get().pop()),
        ..copy *rustc::driver::session::basic_options()
    };

    let mut sess = driver::driver::build_session(sessopts, syntax::diagnostic::emit);

    let mut crate = parse::parse_crate_from_file(cpath, ~[], parsesess);
    // XXX: these need to be kept in sync with the pass order in rustc::driver::compile_rest
    crate = front::config::strip_unconfigured_items(crate);
    crate = syntax::ext::expand::expand_crate(parsesess, ~[], crate);
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
    let rp_set = middle::region::determine_rp_in_crate(sess, ast_map, cmap.def_map, crate);
    let ty_cx = middle::ty::mk_ctxt(sess, cmap.def_map, ast_map, freevars, region_map,
                                    rp_set, lang_items);

    let (mmap, vmap) = middle::typeck::check_crate(ty_cx, copy cmap.trait_map, crate);
    (crate, cmap, ast_map, mmap, vmap, ty_cx)
}


fn main() {
    let cratename = Path(os::args()[1]);
    let (crate, cmap, amap, mmap, vmap, tcx) = get_ast_and_resolve(&cratename);
    let mut v = RustdocVisitor::new();
    v.visit_crate(crate);
    // clean data (de-@'s stuff, ignores uneeded data, stringifies things)
    let mut crate_structs: ~[clean::Struct] = v.structs.iter().transform(|x|
                                                                         x.clean(tcx)).collect();
    // fill in attributes from the ast map
    for crate_structs.mut_iter().advance |x| {
        x.attrs = match amap.get(&x.node) {
            &ast_map::node_item(item, _path) => item.attrs.iter().transform(|x| 
                                                                            x.clean(tcx)).collect(),
            _ => fail!("struct node_id mapped to non-item")
        }
    }

    // convert to json
    for crate_structs.iter().transform(|x| x.to_json()).advance |j| {
        println(j.to_str());
    }
}
