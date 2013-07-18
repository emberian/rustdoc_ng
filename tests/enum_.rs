///An enum representing Fizz or Buzz
pub enum FizzBuzz {
    ///Fizz variant
    Fizz(int),
    ///Buzz variant
    Buzz(int)
}

///My very own result type
pub enum FooBarResult<U,V> {
    /** This operation returned fine */
    FooBarOk(U),
    #[doc = "This operation failed miserably"]
    FooBarErr(V)
}

enum Baz {
    /// struct variant
    A {a: int, b: int},
    /// c-like variant
    B,
    /// unit struct variant
    C {},
    /// basic tuple variant
    D (int, uint, FizzBuzz)
}
