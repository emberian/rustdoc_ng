use clean;

use extra;
use dl = std::unstable::dynamic_lib;

pub type plugin_callback = extern fn (&mut clean::Crate) -> (~str, @extra::json::ToJson);
pub type PluginResult = (~str, @extra::json::ToJson);

/// Manages loading and running of plugins
pub struct PluginManager {
    priv dylibs: ~[dl::DynamicLibrary],
    priv callbacks: ~[plugin_callback],
    /// The directory plugins will be loaded from
    prefix: Path,
}

impl PluginManager {
    /// Create a new plugin manager
    pub fn new(prefix: Path) -> PluginManager {
        PluginManager {
            dylibs: ~[],
            callbacks: ~[],
            prefix: prefix,
        }
    }

    /// Load a plugin with the given name.
    ///
    /// Turns `name` into the proper dynamic library filename for the given
    /// platform. On windows, it turns into name.dll, on OS X, name.dylib, and
    /// elsewhere, libname.so.
    pub fn load_plugin(&mut self, name: ~str) {
        let x = self.prefix.push(libname(name));
        let lib_result = dl::DynamicLibrary::open(Some(&x));
        let lib = lib_result.unwrap();
        let plugin = unsafe { lib.symbol("rustdoc_plugin_entrypoint") }.unwrap();
        self.dylibs.push(lib);
        self.callbacks.push(plugin);
    }

    /// Run all the loaded plugins over the crate, returning their results
    pub fn run_plugins(&self, crate: &mut clean::Crate) -> ~[PluginResult] {
        self.callbacks.iter().transform(|&x| x(crate)).collect()
    }
}

#[cfg(target_os="win32")]
fn libname(mut n: ~str) -> ~str {
    n.push_str(".dll");
    n
}

#[cfg(target_os="macos")]
fn libname(mut n: ~str) -> ~str {
    n.push_str(".dylib");
    n
}

#[cfg(and(not(target_os="win32"), not(target_os="macos")))]
fn libname(n: ~str) -> ~str {
    let mut i = ~"lib";
    i.push_str(n);
    i.push_str(".so");
    i
}
