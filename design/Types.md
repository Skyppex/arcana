# Types

## Structs

Public struct declaration -> Statement

```rs
public struct Foo { // public struct declaration -> Statement
    public bar: int, // public immutable int bar
    public baz: mutable int, // public mutable int baz
}
```

Public mutable struct declaration -> Statement

```rs
public mutable struct Foo { // declaring the struct as mutable makes all fields mutable (you still have to declare the variable as mutable when you use it)
    public bar: int, // public mutable int bar
    public baz: int, // public mutable int baz
}
```

Struct literal -> Expression

```rs
Foo { bar: 5, baz: 10 }
```

Private tuple struct declaration with a field of type Foo called foo -> Statement

```rs
struct Wrapper(foo: Foo);
```

## Enums

Enum declaration with two variants -> Statement

```rs
enum Foo { // public enum declaration -> Statement
    Bar(a: int), // Bar variant with a field of type int called a
    Baz(a: int, b: float), // Baz variant with a field of type int called a and a field of type float called b
}
```

Here we name the fields of each variant explicitly, but you can omit them

```rs
enum Foo {
    Bar(int),
    Baz(int, float),
}
```

### Shared Fields

Enums can have shared fields. Using this constrains each variant to use the same type of syntax for its fields.

```rs
enum Foo(string) {
    Bar(int),
    Baz(int, float),
}
```

The shared field is appended to the end of each variant which mean its equivalent to:

```rs
enum Foo {
    Bar(int, string),
    Baz(int, float, string),
}
```

If the shared field is named, all fields in variants must be named:

```rs
enum Foo(name: string, id: uint) { // Multiple shared fields
    Bar(a: int), // Must have a name
    Baz(a: int, b: float), // Must have a name for each field
}
```

Using shared fields allows you to access that field without caring about which variant a variable is:

```rs
enum Foo(string) {
    Bar(int),
    Baz(int, float),
}

enum NamedFoo(str_id: string) {
    Bar(a: int),
    Baz(a: int, b: float),
}

fn main() {
    let foo = Foo::Bar(-5, "bar"); // Note, you have to assign the shared field here.
    let text = foo.0; // 'foo' now has positional fields. '0' accesses the first shared field on the enum.

    let named_foo = NamedFoo::Baz(-5, .5, "1"); // Note, you have to assign the shared field here.
    let str_id = named_foo.str_id; // When the shared fields are named, you must access them using the fields name.
}
```

## Flags

Public flags type declaration which uses an uint to hold the flags -> Statement
Note that the Foo type is not coercible to an uint, so you can't do `uint foo = Foo::First`

```rs
public flags Foo(uint) {
    First, // default value is 0b0000_0001
    Second, // default value is 0b0000_0010
    Third, // default value is 0b0000_0100
    Fourth, // default value is 0b0000_1000
    Eighth = 0b1000_0000, // value is 0b1000_0000
    FirstAndSecond = First | Second, // value is 0b0011
}

// The type can be omitted and will default to using
// unsinged integers if possible.
public flags Foo { // The underlying type here ends up being int
    First, // default value is 0b0000_0001
    Second, // default value is 0b0000_0010
    Third, // default value is 0b0000_0100
    Fourth, // default value is 0b0000_1000
    Eighth = 0b1000_0000, // value is 0b1000_0000
    FirstAndSecond = First | Second, // value is 0b0011
}
```

### Using flags

Needed checks:

- Check if all of a set of flags are set
- Check if any of a set of flags are set
- Check if only one of a set of flags are set
- Check if all of a set of flags are not set
- Check if any of a set of flags are not set
- Check if only one of a set of flags are not set

## Implementation Blocks for Traits

Implementation blocks are used to implement traits for types.

### Syntax

```rs
imp Trait for Type {
    // Trait function implementations
}
```

### Example

Here the `new` function is implemented for the `Foo` type.

```rs
struct Foo {
    bar: int,
}

imp Foo {
    fun new(bar: int): Foo {
        Foo { bar: bar }
    }
}
```

## Literal Type Annotation

Literal type annotation exists to annotate literal types in situations where just having the actual literal value isn't good enough

### Syntax

```rs
lit a = #"Hello World";

lit b = #255;
```

### Example

Here's an example of when you'd have to use the literal syntax if you wished to specify the generic type.
Note that this example doesn't require the explicit generic annotation because it can be inferred.

Here a union is used as a type constraint. This causes the generic type to only
allow one of the union's variants at a time.
```rs
union Choice { 1, 2, 3 }

struct A<T>
where T is Choice {
    a: T
}

// Here you must specify which variant of the union you want to use
let a = A::<#2> { a: 2 }
let a2 = A::<#2> { a: 3 } // This would fail
```

This is different from allowing any type as a generic parameter and using a
union as the generic type. In this case, the generic type can be any of the
```rs
union Choice { 1, 2, 3 }

struct A<T> {
    a: Choice
}

let a = A::<Choice> { a: 2 } // This would not fail
let a2 = A::<Choice> { a: 3 } // This would not fail
```

## Type Aliases

Type aliases are used to give a type a different name. \
Type aliases are considered their own type, so you can't use them interchangeably.

### Syntax

```rs
type Name = Type;
```

### Example

Here the `Bar` type is an alias for the `Foo` type.

```rs
struct Foo {
    bar: int,
}

type Bar = Foo;
```

You can have implementations for type aliases, but they are not inherited by the type they alias.

```rs
struct Foo {
    bar: int,
}

type Bar = Foo;

implement Bar {
    fn new(bar: int): Bar {
        return Bar { bar: bar };
    }
}

fn main() {
    Bar bar = Bar::new(5), // This works
    Foo foo = Foo::new(5), // This doesn't work
}
```

## Type Parameters (Generics)

Type parameters are used to make a type generic. \
They are similar to generics in other languages.

### Syntax

```rs
struct Foo<T> {
    bar: T,
}
```

### Example

Here the `Foo` type has a type parameter `T` which is used as the type of the `bar` field.

```rs
struct Foo<T> {
    bar: T,
}
```

## Type Constraints

Type constraints are used to restrict the types which can be used as type parameters. \
They are similar to generic constraints in other languages.

### Syntax

```rs
struct Foo<T>
where T is Trait {
    bar: T,
}
```

### Example

Here the `Foo` type has a type parameter `T` which is used as the type of the `bar` field. \
The type parameter `T` must implement the `New` trait.

```rs
trait Newable {
    fn new(): Self;
}

struct Foo<T>
where T is Newable {
    bar: T,
}
```

## Type Arithmetic

Type arithmetic is used to perform arithmetic on types. \
This is useful for generic programming.
Type arithmetic is performed on type aliases, type parameters and type bounds.

### Syntax

This is only a subset of the possible operations.

```rs
type Name = Type - Type.Member;
```

### Examples

Here are some examples of how to use type arithmetic.

#### Example 1

```rs
enum Foo {
    Bar(a: int),
    Baz(a: float),
}

type Exclude<T: enum, ..[U: memberof T]> = T - ..U;

fn main() {
    Foo foo = Foo.Baz;
    Exclude<Foo, Bar> bazOnly = match foo {
        Bar(a) => Exclude<Foo, Bar>.Baz(float.parse(a)),
        Baz(a) => Exclude<Foo, Bar>.Baz(a),
    };
}
```

This lowers to the following:

```rs
enum Foo {
    Bar(a: int),
    Baz(a: float),
}

enum Exclude_Foo_Bar {
    Baz(a: float),
}

fn main() {
    Foo foo = Foo.Baz;
    Exclude_Foo_Bar bazOnly = match foo {
        Bar(a) => Exclude_Foo_Bar.Baz(float.parse(a)),
        Baz(a) => Exclude_Foo_Bar.Baz(a),
    };
}
```

#### Example 2

```rs
struct Foo {
    bar: int,
    baz: float,
}

type Omit<T: struct, ..[U: memberof T]> = T - ..U;

fn main() {
    Foo foo = Foo { bar: 5, baz: 10 };
    Omit<Foo, bar> bazOnly = foo;
    // This is equivalent to Omit<Foo, bar> bazOnly = Omit<Foo, bar> { baz: foo.baz };
}

// You could, for this specific case do this:
type BazOnly = Foo - bar;
// We know, when doing a '-', that we are removeing from Foo.
// This allows us to just specify the field itself, no need to qualify Foo.bar.
```

This lowers to the following:

```rs
struct Foo {
    bar: int,
    baz: float,
}

struct Omit_Foo_bar {
    bar: int,
}

fn main() {
    Foo foo = Foo.Baz;
    Omit_Foo_bar bazOnly = Omit_Foo_bar { baz: foo.baz };
}

struct BazOnly {
    bar: int,
}
```

You can extend types with new fields:

```rs
struct Foo {
    bar: int,
    baz: float,
}

type Extend<T: struct, ..[U: field]> = T + U;

fn main() {
    Foo foo = Foo { bar: 5, baz: 10 };

    // We have to use the 'with' keyword here since a '+' would
    // get confused with the addition operator
    Extend<Foo, barBaz: bool> barBaz = foo with { barBaz: false };
    // This is equivalent to Extend<Foo, barBaz: bool> barBaz = Extend<Foo, barBaz: bool> { baz: foo.baz, barBaz: false };
}
```

This is how you would declare a type like this using another type alias,
you can define the new fields' access modifier.

Here the `Foo` struct has been made `public` along with its fields.
When declaring the `FooBarBaz` type alias, the new `barBaz` field is also marked as `public`.
If you didn't do this, the field would be private for the `FooBarBaz` type even though its itself `public`.

```rs
type Extend<T: struct, ..[U: field]> = T + U;

public struct Foo {
    public bar: int,
    public baz: float,
}

type FooBarBaz = Extend<Foo, public barBaz: bool>;
```

## Easy way to new a Vec

```rs
struct Vec<T> {
    items: ¤[T], // Boxed slice of Ts
}

impl<T> Vec<T> {
    func new<[T]>(items: T): Self {
        Vec { items: ¤items }
    }
}

// Usage
func main() {
    let nums = Vec::new(1, 2, 3);
}
```

In this example the `new` function takes any number of arguments with type `T`. The arguments passed in are converted to a fixed size stack allocated array, in this case `[int; 3]`. The `items` are then boxed and put into the `Vec`.
