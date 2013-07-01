#[link(name = "rustdoc_ng",
       vers = "0.1.0",
       uuid = "8c6e4598-1596-4aa5-a24c-b811914bbbc6")];
#[desc = "rustdoc, the Rust documentation extractor"];
#[license = "MIT/ASL2"];
#[crate_type = "bin"];

#[deny(warnings)];

extern mod syntax;
extern mod rustc;

use rustc::{front, metadata, driver, middle};

use syntax::parse;
use syntax::ast;
use syntax::ast_map;

use std::os;

use visit::*;
use syntax::visit_new::Visitor;

mod visit;
mod doctree;

fn get_ast_and_resolve(crate: &Path) -> (@ast::crate, middle::resolve::CrateMap, ast_map::map) {
    let parsesess = parse::new_parse_sess(None);
    let sessopts = @driver::session::options {
        binary: @"rustdoc",
        maybe_sysroot: Some(@std::os::self_exe_path().get().pop()),
        ..copy *rustc::driver::session::basic_options()
    };

    let sess = driver::driver::build_session(sessopts, syntax::diagnostic::emit);

    let mut crate = parse::parse_crate_from_file(crate, ~[], parsesess);
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
    (crate, cmap, ast_map)
}

fn main() {
    let cratename = Path(os::args()[1]);
    let (crate, cmap, amap) = get_ast_and_resolve(&cratename);
    let v = RustdocVisitor::new();
    v.visit_crate(crate);
}
