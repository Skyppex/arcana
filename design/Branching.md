# Branching

Branching happens when your code can take one of several paths based on a condition.

## If statement

### Syntax

```
if condition {
    body
}
```

## If-else statement

### Syntax

```
if condition {
    body
} else {
    body
}
```

## If-else if-else statement

### Syntax

```
if condition {
    body
} else if condition {
    body
} else {
    body
}
```

## Variable declaration in if statement

### Syntax

```
if condition with variable declaration {
    // value is only available in this scope
    body
}
```
This should also work with `else if`.

## Match statement

The match statement is exhaustive, meaning that all possible cases must be handled.

### Syntax

```
match expression {
    pattern => body,
    pattern => body,
    pattern => body,
    _ => body, // Default case. Must be last, but can be omitted if all cases are handled.
}
```
`match` can return a value:
If the match is assigned to a variable or is directly returned any return statement
in the arm bodies returns for the match statement, not the current function you're in.
```
var value = match expression {
    pattern => body,
    pattern => body,
    pattern => body,
    _ => body, // Default case. Must be last, but can be omitted if all cases are handled.
}
```

### Match works with unions

```
union Foo {
    Bar(i32);
    Baz(i32, f32);
    BarBaz;
}

Foo foo = Foo::Bar(0);

match foo {
    Foo.Bar(int) => body,
    Foo.Baz(int, float) => body,
}
```

### Match works with flags

For flags, the exhaustive match only cares about declared flags, not the underlying type.
This will be possible due to the inability to flip bits which are not declared in the type.
You need to handle all possible combinations of flags.

```
flags Foo(i32) {
    First;
    Second;
    Third;
}

Foo foo = Foo::First;

match foo {
    Foo.First => body, // Exactly First
    Foo.Second | Foo.Third => body, // Exact match where Second and Third are flipped
    Foo.Second or Foo.Third // Exactly Second or exactly Third
    ?Foo.First or ?Foo.Third // As long as First flag or Third flag is flipped
    ?~Foo.First // As long as First isnt flipped (this works because ~ turns 0b0000_0001 into 0b1111_1110) and so it checks any flags which are flipped
}
```
