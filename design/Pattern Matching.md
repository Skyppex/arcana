# Pattern Matching

## Syntax

```rs
<expr> match
| <pattern> => <expr>,
| <pattern> => <expr>,
| _ => {}
```

## Example

```arcana
fun fib(n: int): int => {
    n match
    | <= 0 => 0,
    | 1 => 1,
    | n => fib(n - 1) + fib(n - 2)
}

fun fib2(n: int): int => {
    n match
    | <= 0 => 0,
    | 1 => 1,
    | n => { 
        fib2(n - 1) + fib2(n - 2)
    }
}
```

### Patterns

#### Primitive

- Literal: `1`, `"hello"`, `true`
- Relational: `<= 0`, `> 10`
- Wildcard: `_`
- Variable: `x`

#### Composite

- Or: `<pattern> or <pattern>`
    - `1 or 2` -> matches 1 or 2

- And: `<pattern> and <pattern>`
    - `>= 1 and <= 10` -> matches 1 and 2

#### Destructuring

- Struct `<struct_name> {  <field_name> or <field_name>: <pattern>, ... }`
    - `Point { x, y }` -> defines `x` and `y` variables
    - `Point { x: 1, y: 2 }` -> matches `x` and `y` values to 1 and 2
    - `Point { x: a, y: b }` -> defines `x` and `y` values as `a` and `b`

- Enum `<enum_name>::<variant_name> { <field_name> or <field_name>: <pattern>, ... }`
    - `Option::Some { v }` -> defines `x` variable as value of `Some` variant
    - `Option::Some { v: 1 }` -> matches `Some` variant with value 1
    - `Option::None` -> matches `None` variant

- Enum Variant `<variant_name> { <field_name> or <field_name>: <pattern>, ... }` (same as struct)
    - `Some { v }` -> defines `x` variable as value of `Some` variant
    - `Some { v: 1 }` -> matches `Some` variant with value 1
    - `None` -> matches `None` variant

- Union `<pattern>` (same as primitives, but with a smaller set of possible states)
    - `4` -> matches 4

- Flags `<pattern>` (same as primitives, but with a smaller set of possible states)
    - A flags value can be used to match a set of flags
    - `Mask::First` -> matches 0b0001 exactly
    - `& Mask::First` -> matches 0bXXX1 where X is any value
    - `& Mask::First | Mask::Second` -> matches 0bXX11 where X is any value
    - `? Mask::First | Mask::Second` -> matches 0bXX01, 0bXX10 or 0bXX11 where X is any value
    - `^ Mask::First | Mask::Second` -> matches 0bXX01, 0bXX10 but not 0bXX11 where X is any value
    - `~& Mask::First | Mask::Second` -> matches 0bXX00 where X is any value
    - `~? Mask::First | Mask::Second` -> matches 0bXX01, 0bXX10 or 0bXX00 where X is any value
    - `~^ Mask::First | Mask::Second` -> matches 0bXX01, 0bXX10 but not 0bXX00 where X is any value

