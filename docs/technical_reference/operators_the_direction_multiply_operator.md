# The Direction/Multiply Operator ×

## Monadic Use

`×` is a monadic operator that returns the signum, or sign of the argument.

When the argument is complex, `×` returns the argument divided by its magnitude.

```pometo
× 0 1 ¯2 3
```

```pometo_results
0 1 ¯1 1
```

## Dyadic Use

`×` is a dyadic operator that works in two ways.

If the `rho` of both sides is the same it performs zip-wise multiplication

If either side has a `rho` of [1] it multiplies the value by every value in the other.

Here are some examples:

```pometo
1 2 × 3 4
```

```pometo_results
3 8
```

and

```pometo
1 2 3 4 5 × 33
```

```pometo_results
33 66 99 132 165
```

and

```pometo
2 × 333 444
```

```pometo_results
666 888
```