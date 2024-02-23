# Traits

Traits are a way to add functionality to a class without inheritance. \
They are similar to interfaces in other languages, but with the added functionality of having default implementations.
Some traits are also bound to operators, which allows you to implement operators for your own types.

## Syntax

```
trait Name {
    // Trait body
}
```

## Example

```
trait Add {
    fn add(Self left, Self right): Self;
}
```

In this example, we define a trait called `Add` which has a function called `add` which takes two arguments of the same type as the trait and returns a value of the same type as the trait.
The `Self` keyword is a special keyword which refers to the type which implements the trait.

## Implementing a Trait

```
struct Foo {
    i32 bar;
}

impl Add for Foo {
    fn add(Self left, Self right): Self {
        return Foo { bar: left.bar + right.bar };
    }
}
```

This works because the `i32` type already implements the `Add` trait, so we can use it to implement the trait for our own type.

## Default Implementations

```
trait Equals {
    fn equals(Self left, Self right): bool;
    
    fn not_equals(Self left, Self right): bool {
        return !left.equals(right);
    }
}
```

In this example, we define a trait called `Equals` which has a function called `equals` which takes two arguments of the same type as the trait and returns a value of type `bool`.
We also define a default implementation for the `not_equals` function which returns the opposite of the result of `equals`.
This means that if you implement `equals` for your own type, you get `not_equals` for free.
Implementors of the trait can still override the default implementation if they want to.

## Full Example of the New trait

Defining the `New` trait is quite trivial. We want the trait be implemented only once for each type to avoid ambiguity when calling the new function.

```rs
trait New<[..T: param]> {
    type Return;

    func new([..T]) -> Self::Return;
}
```

This defines the `New` trait as a generic trait which takes an array of possibly different generic types which must all be `param` types.
These generic parameters are then use in the function signature for `new()` which allows the new function to take any amount of different arguments when implemented, but only those exact arguments when called.

When implementing the `New` trait you have to specify the parameters as generic types. Note that you can have zero of them.

```rs
struct MyStruct {
    a: i64,
    b: string,
}

// Would be nice to avoid defining the parameters twice using type inference somehow
impl New<[..a: i64, b: string]> for MyStruct {
    type Return = MyStruct;

    func new(a: i64, b: string): Self::Return {
        MyStruct { a, b }
    }
}

// Here's how you would define it with no input parameters
impl New<[..]> for MyStruct {
    type Return = MyStruct;

    func new(): Self::Return {
        MyStruct { 0, "" }
    }
}
```

Here's how you would constrain your function to take a generic type which has en empty constructor:

```rs
func create_with_extra_logic<T: New<[..]>(): T {
    T::new()
    // Extra logic
}
```