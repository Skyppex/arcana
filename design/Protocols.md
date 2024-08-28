# Protocols

Protocols are a way to add functionality to a class without inheritance. \
They are similar to interfaces in other languages, but with the added functionality of having default implementations.
Some protocols are also bound to operators, which allows you to implement operators for your own types.

## Syntax

```arcana
proto Name {
    // Protocol body
}
```

## Example

```arcana
proto Add {
    fun add(left: Self, right: Self): Self;
}
```

In this example, we define a protocol called `Add` which has a function called `add` which takes two arguments of the same type as the protocol and returns a value of the same type as the protocol.
The `Self` keyword is a special keyword which refers to the type which implements the protocol.

## Implementing a Protocol

```arcana
struct Foo {
    bar: int,
}

imp Add for Foo {
    fun add(Self left, Self right): Self => {
        return Foo { bar: left.bar + right.bar };
    }
}
```

This works because the `int` type already implements the `Add` protocol, so we can use it to implement the protocol for our own type.

## Default Implementations

```arcana
proto Equals {
    fun equals(Self left, Self right): bool;
    
    fun not_equals(Self left, Self right): bool => {
        return !left.equals(right);
    }
}
```

In this example, we define a protocol called `Equals` which has a function called `equals` which takes two arguments of the same type as the protocol and returns a value of type `bool`.
We also define a default implementation for the `not_equals` function which returns the opposite of the result of `equals`.
This means that if you implement `equals` for your own type, you get `not_equals` for free.
Implementors of the protocol can still override the default implementation if they want to.

## Full Example of the Newable protocol

Defining the `Newable` protocol is quite trivial. We want the protocol be implemented only once for each type to avoid ambiguity when calling the new function.

```arcana
proto Newable<[..T: param]> {
    type Return;

    fun new([..T]): Self::Return;
}
```

This defines the `Newable` protocol as a generic protocol which takes any number of possibly different generic types which must all be `param` types.

> The *possibly different* part of it from the `..` syntax.
> If it was omitted it would take any number of the **same** generic type. 

> Also note that the generic `[T]` is coercible to `[..T]` because the *possibly different* types can theoretically be the same type.

These generic parameters are then use in the function signature for `new()` which allows the new function to take any amount of different arguments when implemented, but only those exact arguments when called.

When implementing the `Newable` protocol you have to specify the parameters as generic types. Note that you can have zero of them.

```arcana
struct MyStruct {
    a: int,
    b: string,
}

// Would be nice to avoid defining the parameters twice using type inference somehow
imp Newable<[..a: int, b: string]> for MyStruct {
    type Return = MyStruct;

    fun new(a: int, b: string): Self::Return => {
        MyStruct { a: a, b: b }
    }
}

// Here's how you would define it with no input parameters
imp Newable<[..]> for MyStruct {
    type Return = MyStruct;

    func new(): Self::Return => {
        MyStruct { a: 0, b: "" }
    }
}
```

Here's how you would constrain your function to take a generic type which has en empty constructor:

```arcana
fun create_with_extra_logic<T>(): T
where T: Newable<[..]> => {
    T::new()
    // Extra logic
}
```
