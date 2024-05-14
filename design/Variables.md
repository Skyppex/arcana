# Syntax Examples

`let x: int;` Variable declaration -> Statement \
`let mut x: int;` Mutable variable declaration -> Statement \
`let x = 0;` Variable declaration with initialization -> Expression \
`let mut x: int = 0;` Mutable variable declaration with initialization -> Expression

`x = 0;` Variable assignment -> Expression

## Literals

`0` int literal -> Expression \
`0i` int literal -> Expression \
`0u` uint literal -> Expression \
`0.0` float literal -> Expression \
`0.0f` float literal -> Expression \
`true` Boolean literal -> Expression \
`false` Boolean literal -> Expression \
`'c'` Character literal -> Expression \
`"Hello World"` String literal -> Expression \
`i"Hello {name}"` Interpolated string literal -> Expression \
`r#"Hello "World""#` Raw string literal -> Expression \

Raw interpolated string literal -> Expression

```rs
r"Hello
---"World"
-"
```

Raw multiline string literal with different delimiters -> Expression

```rs
r#"Hello
---"World"
--"#
```

### Struct literals

Given the following struct:

```rs
struct Foo {
    bar: i32;
    baz: f32;
}
```

The struct literal syntax is as follows:

```rs
Foo { bar: 5, baz: 10 }
```

This is assignable to a value of type Foo

## Defining literal values

```rs
lit x = 1;
```
