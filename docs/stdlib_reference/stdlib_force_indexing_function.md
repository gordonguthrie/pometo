# The stdlib function `⎕force_indexing`

## Runtime forcing of indexing

A `Pometo` shape is defined by this Erlang record:

```
-record('$shape¯', {
                    shaping    = eager, % [eager | lazy]
                    indexed    = false,
                    dimensions = [],
                    forcing    = none,
                    type       = none,
                    line_no    = none,
                    char_no    = none
                   }).
```

The field `forcing` which by default has a value of `none` can be used to hint to the runtime as how to proceed. If the data structure is `unindexed` but will be needed to consumed as an `indexed` data structure the value of `forcing` will be set to `indexed` and at the next traversal of the data structure its internal representation will be changed.

This function allows Developers working on the `Pometo` runtime (and not people ordinarily writing `Pometo` programmes to toggle data structures for the purposes of testing.

For more details please see the discussion in [the discussion of ⎕make_indexed](../stdlib_make_indexed_function.html).