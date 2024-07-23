# Pattern Matching

## Syntax

```rs
match <expr>
arm <pattern> => <expr>,
arm <pattern> => <expr>,
arm _ => {}
```

## Example

```rs
fun fib(int n): int => {
    match n
    arm <= 0 => 0,
    arm 1 => 1,
    arm n => fib(n - 1) + fib(n - 2)
}

fun fib2(int n): int => {
    match n
    arm <= 0 => 0,
    arm 1 => 1,
    arm n => { 
        fib2(n - 1) + fib2(n - 2)
    }
}
```
