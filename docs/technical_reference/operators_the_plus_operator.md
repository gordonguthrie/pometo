# The Plus Operator +

## Dyadic Use

`+` is a diadic operator that works in two ways.

If the `rho` of both sides is the same it performs zip-wise addition

If either side has a `rho` of [1] it adds the value to every value in the other.

Here are some examples:

```pometo
1 2 + 3 4
```

```pometo_results
4 6
```

and

```pometo
1 2 3 4 5 + 33
```

```pometo_results
34 35 36 37 38
```

and

```pometo
1 + 22 33 44 55
```

```pometo_results
23 34 45 56
```
