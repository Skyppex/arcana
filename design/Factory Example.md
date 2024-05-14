# Factory Example

Lets say you're creating a library which implements a generic factory pattern that the consumers of your library uses to create instances of their own types.
You might have some complex functionality already, but your users also want to just call a simple function and give it the same arguments as they would in their own implementation of the `new` function.

Here's how you might provide a function like that:

The consumers code:

```rs
#[derive(Newable)] // Auto implements a `new` function which takes in `a: int` and `b: string` and assigns them to the fields.
struct MyStruct {
    a: int,
    b: string
}
```

Here comes your `create` function:

```rs
impl<T> Create<T> for T {
    pub func create<[..P]>(args: [..P]): T
    where T is Newable<[..P]> {
        T::new(args)
    }
}
```

When the function is used it looks like this:

```rs
let my_struct = MyStruct::create(5, "Five");
```

You would likely want to have additional functionality with your factory function and in such a case you might need additional parameters.
The unrolling of parameters as in the above example must happen at the end of the function signature. This means that
if you want to add your own parameters you'd have to add them at the beginning.

```rs
impl<T> Create<T> for T {
    pub func create<[..P]>(x: int, y: int, do: bool, args: [..P]): T
    where T is Newable<[..P]> {
        // Do some stuff here which uses the extra arguments and interacts with the rest of your factory code in some way.
        // if do {
        //     x + y
        // }
        // And other useful code
        T::new(args)
    }
}
```
