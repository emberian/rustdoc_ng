/// A Foo
struct Foo {
    /// Is this Foo weak?
    feeble: bool,
    /// How many bars this Foo owns
    bars: uint,
    undoc: Bar
}

struct Bar {
    a: uint,
    b: int,
    c: (int, uint, Baz)
}

#[doc="foo"]
struct Baz {
    /// a field
    a: uint
}

struct Qux;

struct Quux(/** doc */ int, uint, Baz);

fn main() {}
