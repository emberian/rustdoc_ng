use clean;
use plugins;

pub fn noop(_crate: &mut clean::Crate) -> plugins::PluginResult {
    None
}

pub fn strip_hidden(_crate: &mut clean::Crate) -> plugins::PluginResult {
    None
}

pub fn clean_comments(_crate: &mut clean::Crate) -> plugins::PluginResult {
    None
}
