
# Generics

Generics can be used on type declarations (only structs for now)
fields on struct can use the generic type which was declared on the struct

```rs
struct A<T> {
    a: T
}
```

When a generic struct declaration is encountered during type discovery, record its structure including the generic types and its fields which might use the generics for their type

When a generic struct declaration is encountered during type checking, create a child type environment for the struct and use that when looking up types which can be used on the fields.

Add the generic struct with its generics to the parent type environment
Add the fields to the same parent type environment
    This should include the struct which the field is declared in including the structs generics and also the fields generic type

When a generic struct with a concrete type is encountered when declaring a variable for example
find the generic version of the struct in the type environment
create a clone of the generic struct substituting the generics with the concrete types in both the struct declaration and the fields
to do this, create a table going from the generic type to the concrete type that can be reused

```rs
let a: A<bool>;
```