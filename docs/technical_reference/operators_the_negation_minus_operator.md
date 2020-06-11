# The Negation/Minus Operator -

## Monadic Use

`-` is a monadic operator that returns the `negation` of the argument.

```pometo
- 1 3 5
```

```pometo_results
¯1 ¯3 ¯5
```

```pometo
- ¯2 4 ¯6
```

```pometo_results
2 ¯4 6
```

## Dyadic Use

`-` is a dyadic operator that works in two ways.

If the `rho` of both sides is the same it performs zip-wise subtraction

If either side has a `rho` of [1] it subtracts the value from every value in the other.

Here are some examples:

```pometo
1 2 - 3 4
```

```pometo_results
¯2 ¯2
```

and

```pometo
 1 2 3 4 5 - 33
```

```pometo_results
¯32 ¯31 ¯30 ¯29 ¯28
```

and

```pometo
1 - 22 33 44 55
```

```pometo_results
¯21 ¯32 ¯43 ¯54
```
