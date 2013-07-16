///foo it up
pub fn foo(arg0: ~int) {
    println("hi");
}

///bar it up
priv fn bar() -> @bool {
   @true
}

//this function can only fail
/*fn always_fail() -> ! {
    fail!("it failed.")
}
*/
///main cause we're boring
fn main() { }
