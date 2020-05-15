# The Plus Operator ÷

## Dyadic Use

`÷` is a diadic operator that works in two ways.

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
