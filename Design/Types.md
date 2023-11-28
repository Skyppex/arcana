# Types

## Structs

Public struct declaration -> Statement
```
public struct Foo {
    public i32 bar; // public immutable i32 bar
    public mutable i32 baz; // public mutable i32 baz
}
```

Struct literal -> Expression
```
Foo { bar: 5, baz: 10 }
```

Private tuple struct declaration with a field of type Foo called foo -> Statement
```
struct Wrapper(Foo foo);
struct Wrapper(mutable Foo foo); // mutable tuple struct declaration
```

## Unions

Union declaration with two variants -> Statement
```
public union Foo { // public union declaration -> Statement
    Bar(i32 int); // Bar variant with a field of type i32 called int
    Baz(i32 int, f32 float); // Baz variant with a field of type f32 called float
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
    i32 bar;
}

implement Foo {
    fn new(i32 bar): Foo {
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
    i32 bar;
}

type Bar = Foo;
```

You can have implementations for type aliases, but they are not inherited by the type they alias.
```
struct Foo {
    i32 bar;
}

type Bar = Foo;

implement Bar {
    fn new(i32 bar): Bar {
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
    T bar;
}
```

### Example

Here the `Foo` type has a type parameter `T` which is used as the type of the `bar` field.
```
struct Foo<T> {
    T bar;
}
```

## Type Constraints

Type constraints are used to restrict the types which can be used as type parameters. \
They are similar to generic constraints in other languages.

### Syntax

```
struct Foo<T: Trait> {
    T bar;
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
    T bar;
}
```

## Type Bounds

Type bounds are used to restrict the types which can be used as type parameters. \
They are similar to generic constraints in other languages.

### Syntax

```
struct Foo<T> where T: Trait {
    T bar;
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
    T bar;
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

### Example

```
type Exclude<T: union, U: memberof T> = T - U;
```
