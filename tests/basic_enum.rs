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
