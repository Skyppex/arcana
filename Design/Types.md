# Types

## Structs

Public struct declaration -> Statement
```
public struct Foo { // public struct declaration -> Statement
    public bar: i32; // public immutable i32 bar
    public baz: mutable i32; // public mutable i32 baz
}
```

Public mutable struct declaration -> Statement
```
public mutable struct Foo { // declaring the struct as mutable makes all fields mutable (you still have to declare the variable as mutable when you use it)
    public bar: i32; // public mutable i32 bar
    public baz: i32; // public mutable i32 baz
}
```

Struct literal -> Expression
```
Foo { bar: 5, baz: 10 }
```

Private tuple struct declaration with a field of type Foo called foo -> Statement
```
struct Wrapper(foo: Foo);
struct Wrapper(foo: mutable Foo); // mutable tuple struct declaration
```

## Unions

Union declaration with two variants -> Statement
```
public union Foo { // public union declaration -> Statement
    Bar(a: i32); // Bar variant with a field of type i32 called a
    Baz(a: i32, b: f32); // Baz variant with a field of type i32 called a and a field of type f32 called b
}
```

## Flags

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

## Implementation Blocks for Types

Implementation blocks are used to implement functions or traits for types.

### Syntax

```
implement Type {
    // Functions and traits
}
```

### Example

Here the `new` function is implemented for the `Foo` type.
```
struct Foo {
    bar: i32;
}

implement Foo {
    fn new(bar: i32): Foo {
        return Foo { bar: bar };
    }
}
```

## Type Aliases

Type aliases are used to give a type a different name. \
Type aliases are considered their own type, so you can't use them interchangeably.

### Syntax

```
type Name = Type;
```

### Example

Here the `Bar` type is an alias for the `Foo` type.
```
struct Foo {
    bar: i32;
}

type Bar = Foo;
```

You can have implementations for type aliases, but they are not inherited by the type they alias.
```
struct Foo {
    bar: i32;
}

type Bar = Foo;

implement Bar {
    fn new(bar: i32): Bar {
        return Bar { bar: bar };
    }
}

fn main() {
    Bar bar = Bar::new(5); // This works
    Foo foo = Foo::new(5); // This doesn't work
}
```

## Type Parameters

Type parameters are used to make a type generic. \
They are similar to generics in other languages.

### Syntax

```
struct Foo<T> {
    bar: T;
}
```

### Example

Here the `Foo` type has a type parameter `T` which is used as the type of the `bar` field.
```
struct Foo<T> {
    bar: T;
}
```

## Type Constraints

Type constraints are used to restrict the types which can be used as type parameters. \
They are similar to generic constraints in other languages.

### Syntax

```
struct Foo<T: Trait> {
    bar: T;
}
```

### Example

Here the `Foo` type has a type parameter `T` which is used as the type of the `bar` field. \
The type parameter `T` must implement the `Newable` trait.
```
trait Newable {
    fn new(): Self;
}

struct Foo<T: Trait> {
    bar: T;
}
```

## Type Bounds

Type bounds are used to restrict the types which can be used as type parameters. \
They are similar to generic constraints in other languages.

### Syntax

```
struct Foo<T> where T: Trait {
    bar: T;
}
```

### Example

Here the `Foo` type has a type parameter `T` which is used as the type of the `bar` field. \
The type parameter `T` must implement the `Newable` trait.
```
trait Newable {
    fn new(): Self;
}

struct Foo<T> where T: Trait {
    bar: T;
}
```

## Type Arithmetic

Type arithmetic is used to perform arithmetic on types. \
This is useful for generic programming.
Type arithmetic is performed on type aliases, type parameters and type bounds.

### Syntax

This is only a subset of the possible operations.
```
type Name = Type - Type.Member;
```

### Examples

Here are some examples of how to use type arithmetic.

#### Example 1
```
union Foo {
    Bar(a: i32);
    Baz(a: f32);
}

type Exclude<T: union, [U: memberof T]> = T - U;

fn main() {
    Foo foo = Foo.Baz;
    Exclude<Foo, Bar> bazOnly = match foo {
        Bar(a) => Exclude<Foo, Bar>.Baz(f32.parse(a)),
        Baz(a) => Exclude<Foo, Bar>.Baz(a),
    };
}
```

This lowers to the following:
```
union Foo {
    Bar(a: i32);
    Baz(a: f32);
}

union Exclude_Foo_Bar {
    Baz(a: f32);
}

fn main() {
    Foo foo = Foo.Baz;
    Exclude_Foo_Bar bazOnly = match foo {
        Bar(a) => Exclude_Foo_Bar.Baz(f32.parse(a)),
        Baz(a) => Exclude_Foo_Bar.Baz(a),
    };
}
```

#### Example 2

```
struct Foo {
    bar: i32;
    baz: f32;
}

type Omit<T: struct, [U: memberof T]> = T - U;

fn main() {
    Foo foo = Foo { bar: 5, baz: 10 };
    Omit<Foo, bar> bazOnly = foo; // This is equivalent to Omit<Foo, bar> bazOnly = Omit<Foo, bar> { baz: foo.baz };
}
```

This lowers to the following:
```
struct Foo {
    bar: i32;
    baz: f32;
}

struct Omit_Foo_bar {
    bar: i32;
}

fn main() {
    Foo foo = Foo.Baz;
    Omit_Foo_bar bazOnly = Omit_Foo_bar { baz: foo.baz };
}
```

#### Example 3

```
struct Foo {
    bar: i32;
    baz: f32;
}
