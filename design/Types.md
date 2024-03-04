# Types

## Structs

Public struct declaration -> Statement
```rs
public struct Foo { // public struct declaration -> Statement
    public bar: i32, // public immutable i32 bar
    public baz: mutable i32, // public mutable i32 baz
}
```

Public mutable struct declaration -> Statement
```rs
public mutable struct Foo { // declaring the struct as mutable makes all fields mutable (you still have to declare the variable as mutable when you use it)
    public bar: i32, // public mutable i32 bar
    public baz: i32, // public mutable i32 baz
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
    Bar(a: i32), // Bar variant with a field of type i32 called a
    Baz(a: i32, b: f32), // Baz variant with a field of type i32 called a and a field of type f32 called b
}
```
Here we name the fields of each variant explicitly, but you can omit them
```rs
enum Foo {
    Bar(i32),
    Baz(i32, f32),
}
```

### Shared Fields
Enums can have shared fields. Using this constrains each variant to use the same type of syntax for its fields.

```rs
enum Foo(string) {
    Bar(i32),
    Baz(i32, f32),
}
```
The shared field is appended to the end of each variant which mean its equivalent to:
```rs
enum Foo {
    Bar(i32, string),
    Baz(i32, f32, string),
}
```

If the shared field is named, all fields in variants must be named:
```rs
enum Foo(name: string, id: u64) { // Multiple shared fields
    Bar(a: i32), // Must have a name
    Baz(a: i32, b: f32), // Must have a name for each field
}
```

Using shared fields allows you to access that field without caring about which variant a variable is:
```rs
enum Foo(string) {
    Bar(i32),
    Baz(i32, f32),
}

enum NamedFoo(str_id: string) {
    Bar(a: i32),
    Baz(a: i32, b: f32),
}

fn main() {
    let foo = Foo::Bar(-5, "bar"); // Note, you have to assign the shared field here.
    let text = foo.0; // 'foo' now has positional fields. '0' accesses the first shared field on the enum.

    let named_foo = NamedFoo::Baz(-5, .5, "1"); // Note, you have to assign the shared field here.
    let str_id = named_foo.str_id; // When the shared fields are named, you must access them using the fields name.
}
```

## Flags

Public flags type declaration which uses an u32 to hold the flags -> Statement
Note that the Foo type is not coercible to an u32, so you can't do `u32 foo = Foo::First`
```rs
public flags Foo(u32) {
    First, // default value is 0b0000_0001
    Second, // default value is 0b0000_0010
    Third, // default value is 0b0000_0100
    Fourth, // default value is 0b0000_1000
    Eighth = 0b1000_0000, // value is 0b1000_0000
    FirstAndSecond = First | Second, // value is 0b0011
}

// The type can be omitted and will default to the smallest possible integer type using
// unsinged integers if possible.
public flags Foo { // The underlying type here ends up being u8
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
- Check if all of a set of flags are not set
- Check if any of a set of flags are not set

## Implementation Blocks for Types

Implementation blocks are used to implement functions or traits for types.

### Syntax

```rs
implement Type {
    // Functions and traits
}
```

### Example

Here the `new` function is implemented for the `Foo` type.
```rs
struct Foo {
    bar: i32,
}

implement Foo {
    fn new(bar: i32): Foo {
        return Foo { bar: bar };
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

```rs
struct A<T>
where T is 1 or 2 or 3 {
    a: T
}

let a = A::<#2> { a: 2 }
```

```rs
struct A {
    a: 1 or 2 or 3
}

let a = A { a: 2 }
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
    bar: i32,
}

type Bar = Foo;
```

You can have implementations for type aliases, but they are not inherited by the type they alias.
```rs
struct Foo {
    bar: i32,
}

type Bar = Foo;

implement Bar {
    fn new(bar: i32): Bar {
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
    Bar(a: i32),
    Baz(a: f32),
}

type Exclude<T: enum, ..[U: memberof T]> = T - ..U;

fn main() {
    Foo foo = Foo.Baz;
    Exclude<Foo, Bar> bazOnly = match foo {
        Bar(a) => Exclude<Foo, Bar>.Baz(f32.parse(a)),
        Baz(a) => Exclude<Foo, Bar>.Baz(a),
    };
}
```

This lowers to the following:
```rs
enum Foo {
    Bar(a: i32),
    Baz(a: f32),
}

enum Exclude_Foo_Bar {
    Baz(a: f32),
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

```rs
struct Foo {
    bar: i32,
    baz: f32,
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
    bar: i32,
    baz: f32,
}

struct Omit_Foo_bar {
    bar: i32,
}

fn main() {
    Foo foo = Foo.Baz;
    Omit_Foo_bar bazOnly = Omit_Foo_bar { baz: foo.baz };
}

struct BazOnly {
    bar: i32,
}
```

You can extend types with new fields:
```rs
struct Foo {
    bar: i32,
    baz: f32,
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
    public bar: i32,
    public baz: f32,
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

In this example the `new` function takes any number of arguments with type `T`. The arguments passed in are converted to a fixed size stack allocated array, in this case `[i64; 3]`. The `items` are then boxed and put into the `Vec`.