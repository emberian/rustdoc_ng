This is a prototype of rustdoc. It uses `rustpkg`, so clone the repo into a
workspace (I use `~/.rustpkg`, so it goes into `~/.rustpkg/src`) and run
`rustpkg build rustdoc_ng`. As a temporary hack, you need to move the
executable (`rustdoc_ng`) into your Rust prefix (I use `../configure
--prefix=~/.local`, so I put it in `~/.local/bin`).

**Do not use unless you plan on helping develop it!** It is incomplete and
can't actually generate documentation yet.

The basic structure is that `visit` extracts relevant data from the Rust AST
as given by libsyntax. `clean` then takes this simplified AST and stringifies
it and resolves types and such. From there, pluggable "filters" can be run
over this clean AST to remove or modify nodes, as well as augment the data
included.  As the last step, the cleaned AST is jsonified and returned.
Plugins can return a name and `@ToJson` trait object, and their information
will be included in the JSON output under the given name.

Copyright 2013 Corey Richardson

Licensed under the Apache License, Version 2.0
<http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
<http://opensource.org/licenses/MIT>, at your option. All files in the project
carrying such notice may not be copied, modified, or distributed except
according to those terms.
