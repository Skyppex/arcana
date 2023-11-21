```
public struct Foo { // public struct declaration -> Statement
    public i32 bar; // public immutable i32 bar
    public mutable i32 baz; // public mutable i32 baz
}
```

Private tuple struct declaration with a field of type Foo called foo -> Statement
```
struct Wrapper(Foo foo);
struct Wrapper(mutable Foo foo); // mutable tuple struct declaration
```

Union declaration with two variants -> Statement
```
public union Foo { // public union declaration -> Statement
    Bar(i32 int); // Bar variant with a field of type i32 called int
    Baz(i32 int, f32 float); // Baz variant with a field of type f32 called float
}
```

Public flags type declaration which uses an u32 to hold the flags -> Statement
Note that the Foo type is not coercible to an u32, so you can't do `u32 foo = Foo::First`
```
public flags Foo(u32) {
    First; // default value is 0b0000_0001
    Second; // default value is 0b0000_0010
    Third; // default value is 0b0000_0100
    Fourth; // default value is 0b0000_1000
    Eighth = 0b1000_0000; // value is 0b1000_0000
    FirstAndSecond = First | Second; // value is 0b0011
}

// The type can be omitted and will default to the smallest possible integer type using
// unsinged integers if possible.
public flags Foo { // The underlying type here ends up being u8
    First; // default value is 0b0000_0001
    Second; // default value is 0b0000_0010
    Third; // default value is 0b0000_0100
    Fourth; // default value is 0b0000_1000
    Eighth = 0b1000_0000; // value is 0b1000_0000
    FirstAndSecond = First | Second; // value is 0b0011
}

```
