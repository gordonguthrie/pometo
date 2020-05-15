# The Minus Operator ¯

`¯` is a diadic operator that works in two ways.

If the `rho` of both sides is the same it performs zip-wise subtraction

If either side has a `rho` of [1] it adds the value to every value in the other.

Here are a couple of examples:

```pometo
1 2 ¯ 3 4
```

```pometo_results
-2 -2
```

and

```pometo
1 2 3 4 5 ¯ 33
```

```pometo_results
¯32 ¯31 ¯30 ¯29 ¯28
```
