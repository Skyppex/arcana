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