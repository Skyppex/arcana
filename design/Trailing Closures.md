# Trailing Closures

Trailing closures allows you to extract a closure from the argument list to
outside the parentheses. This is only possible if the closure is the last
argument in the argument list.

## Syntax

```arcana
fun foo(a: int, b: int, c: fun(int): int): int => {
    return c(a) + b;
}

// Normal call with a closure in the argument list
foo(1, 2, |x| x + 1)

// Trailing closure
foo(1, 2) |x| -> x + 1

foo(1, 2) -> |x| x + 1
```
