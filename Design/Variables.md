# Syntax Examples

`i32 x;` Variable declaration -> Statement \
`mutable i32 x;` Mutable variable declaration -> Statement \
`i32 x = 0;` Variable declaration with initialization -> Expression \
`mutable i32 x = 0;` Mutable variable declaration with initialization -> Expression

`x = 0;` Variable assignment -> Expression

# Literals
`0i8` i8 literal -> Expression \
`0u8` u8 literal -> Expression \
`0i16` i16 literal -> Expression \
`0u16` u16 literal -> Expression \
`0` i32 literal -> Expression \
`0u` u32 literal -> Expression \
`0i32` i32 literal -> Expression \
`0u32` u32 literal -> Expression \
`0i64` i64 literal -> Expression \
`0u64` u64 literal -> Expression \
`0i128` i128 literal -> Expression \
`0u128` u128 literal -> Expression \
`0isize` isize literal -> Expression \
`0usize` usize literal -> Expression \
`0.0f8` f8 literal -> Expression \
`0.0f16` f16 literal -> Expression \ 
`0.0` f32 literal -> Expression \
`0.0f32` f32 literal -> Expression \
`0.0f64` f64 literal -> Expression \
`0.0f128` f128 literal -> Expression \
`true` Boolean literal -> Expression \
`false` Boolean literal -> Expression \
`'c'` Character literal -> Expression
`"Hello World"` String literal -> Expression \
`r"Hello {name}"` Interpolated string literal -> Expression \
`r#"Hello "World""#` Raw string literal -> Expression \

Raw interpolated string literal -> Expression
```
r"Hello
---"World"
-"
```

Raw multiline string literal with different delimiters -> Expression
```
r#"Hello
---"World"
--"#
```

### Struct literals

Given the following struct:
```
struct Foo {
    i32 bar;
    f32 baz;
}
```

The struct literal syntax is as follows:
```
Foo { bar: 5, baz: 10 }
```
This is assignable to a value of type Foo