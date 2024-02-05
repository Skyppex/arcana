# Modules

Modules are completely seperated from the file system and is unopinionated about how you organize your project.

At the top of each file, after any use statements, a `mod` statement is required.

```rs
mod project;
```

This syntax declares the entire file as part of the `project` module.

A module can be a block which scopes the module to only stuff inside the block.

```rs
mod project {
    // Code here is inside the 'project' module
}
```

Modules can be nested.

```rs
mod project {
    // Code here is inside the 'project' module

    mod system {
        // Code here is inside the 'project::system' module
    }
}
```

This also works with file scoped modules.

```rs
mod project;
// Code here is inside the 'project' module

mod system {
    // Code here is inside the 'project::system' module
}
```

In this example both modules are file scoped.
This will put each following module statement inside the top most one.
```rs
mod project;
// Code here is inside the 'project' module

mod system;
// Code here is inside the 'project::system' module
```

These are both inside the `project` module
```rs
mod project;
// Code here is inside the 'project' module

mod system;
// Code here is inside the 'project::system' module

mod other_system;
// Code here is inside the 'project::other_system' module
```

If you wanted a structure where the `other_system` module was
inside the `system` module you'd have to use blocks.

## Accessability

Different access modifiers allow you to control what parts of your code are exposed to other parts of your code or an external consumer of your API.

- `public`
    The `public` access modifier exposes this module to everything.
- `internal`
    The `internal` access modifier exposes this module to your entire project or package, but is not public to external consumers of your API.
- `super`
    The `super` access modifier exposes this module to its parent module only

There is no `private` keyword as it is implicit on everything which doesn't have any of the above access modifiers on them. This way everything is closed by default and there is need to think about it.

Access modifiers can be used on more than just modules. Here is a comprehensive list of all legal uses of access modifiers:

- Modules
- Types (struct, union, flags, type)
- Struct fields
    - type statements which extend structs will inherit access from the structs fields
    - type statements which add new fields to a struct allow you to define the access inline
- Traits
- Functions
- Notably, not Impl blocks (you use it on the functions instead)