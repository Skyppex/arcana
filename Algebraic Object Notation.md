# Algebraic Object Notation (AON)

This is my attempt at an object notation format.

## Values

### Null

No value -> `null`


### Boolean

Either `true` or `false`


### Number

Any real number -> `1`, `1.0`

### String

Any text -> `"Hello World"`

### Struct

A collection of key-value pairs -> `{ "key": "value" }`
```
{
    "key": "value",
    "key2": "value2"
}
```

### Union

A union is a collection where only one value can be present at a time. It is represented as a struct with a special key called `type` that specifies the type of the union.
`type1` is one of the discriminants.
```
#type1 {
    "key": "value"
}
```

Alternative syntax for better compatibility:
```
{
    "__uniontype__": "type1",
    "key": "value"
}
```
This syntax allows a normal JSON parser to parse the object, but won't output the correct results without a custom deserializer.

### Array

A collection of values -> `[ "value1", "value2" ]`
```
[
    "value1",
    "value2"
]
```

### Features

The syntax for JSON is a subset of the syntax for AON. This means that any valid JSON is also valid AON.

The syntax allows for trailing commas in structs, arrays and unions.
```
{
    "key": "value",
    "key2": "value2",
}
```

### Examples

Here are our models in pseudo code:
```
struct MyStruct {
    nullValue: null,
    boolValue: boolean,
    numberValue: number,
    stringValue: string,
    structValue: Option<MyStruct>,
}

union Option<T> {
    Some(T),
    None,
}
```

Here are some representations of MyStruct with some sample values:

```aon
{
  "nullValue": null,
  "boolValue": true,
  "numberValue": 14,
  "stringValue": "Foo",
  "structValue": #some {
    "nullValue": null,
    "boolValue": false,
    "numberValue": -1.618,
    "stringValue": "Bar",
    "structValue": #none {}
  }
}
```

If a union variant has no fields, the braces can be omitted:
```aon
{
  "nullValue": null,
  "boolValue": false,
  "numberValue": -1.618,
  "stringValue": "Bar",
  "structValue": #none
}
```
