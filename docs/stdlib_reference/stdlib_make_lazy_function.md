# The stdlib function `⎕make_lazy`

## Lazy and eager shapes

A normal `Pometo` shape is an eager vector - it knows how long it is:

It is defined by this Erlang record:

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

A normal eager vector `1 2 3` looks like this internally - `shaping` is `eager` and the `dimensions` are known:

```erlang
{'$ast¯',{'$shape¯',eager,false,[3],none,number,1,19},[1,2,3],1,19}
```

The `stdlib` function `⎕make_indexed` takes one of these and converts it to a lazy vector, one which doesn't know how long it is:

```erlang
{'$ast¯',{'$shape¯',lazy,false,unsized_vector,none,number,1,16},
                [1,2,3],
                1,16}
```

For more details please see the discussion in [the discussion of ⎕make_indexed](./stdlib_make_indexed_function.md).