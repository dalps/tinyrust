## State design

- For stack values, a stack of environments mapping ide to stack values.
- For heap values, a dedicated memory.
- The ownership relationship, from values to ide, injective, partial.

```
type t = {
    envstack : env Stack.t;
    mutable heap : mem;
    module_env : env;
    mutable ownership : memval Map.Make(String).t;
  }
```


* Just a stack of (ide, value) bindings:
  - what are the blocks delimiters?


* A stack of stacks
  - inheriting the current context is expensive
    because it require copying the stack


* A stack of functions
  - seems like a good idea




