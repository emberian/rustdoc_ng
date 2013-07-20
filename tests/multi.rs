#[link(name="test1")];
extern mod extra;

///It holds fads
struct Fads<T,U> {
    ///hurr
    asdf: ~str,
    /**splort*/
    hjkl: bool,
#[doc = "abadog"]
    quux: Option<Result<T,U>>,
}

///squawk!
fn squawk() -> bool {
    false
}

/**
 * This struct is called Blegh.
 * bleeeegh
 * double bleeegh
 */
struct Blegh<'self> {
    ///this is a fads
    foo: &'self Fads<char, char>,
    ///this is a buzzy
    buzzy: bool,
}

enum Hoohah<'self> {
    Hoo(Blegh<'self>),
    Hah(Option<Option<Option<char>>>),
}
