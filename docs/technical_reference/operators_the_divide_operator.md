# The Plus Operator ÷

## Monadic Use
`÷` is a monadic operator that returns the reciprocal of the argument.

```pometo
÷ 4 5 10
```

```pometo_results
0.25 0.2 0.1
```

## Dyadic Use

`÷` is a dyadic operator that works in two ways.

If the `rho` of both sides is the same it performs zip-wise division

If either side has a `rho` of [1] it divides the value by every value in the other.

Here are some of examples:

```pometo
60 30 ÷ 12 5
```

```pometo_results
5.0 6.0
```

and

```pometo
10 20 30 40 ÷ 5
```

```pometo_results
2.0 4.0 6.0 8.0
```

```pometo
100 ÷ 10 20
```

```pometo_results
10.0 5.0
```
