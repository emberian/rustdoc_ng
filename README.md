This is a prototype of rustdoc. It uses `rustpkg`, so clone the repo into a
workspace (I use `~/.rustpkg`, so it goes into `~/.rustpkg/src`) and run
`rustpkg build rustdoc_ng`. As a temporary hack, you need to move the
executable (`rustdoc_ng`) into your Rust prefix (I use `../configure
--prefix=~/.local`, so I put it in `~/.local/bin`).

**Do not use unless you plan on helping develop it!**

Copyright 2013 Corey Richardson

Licensed under the Apache License, Version 2.0
<http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
<http://opensource.org/licenses/MIT>, at your option. All files in the project
carrying such notice may not be copied, modified, or distributed except
according to those terms.
