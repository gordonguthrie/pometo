# The Reciprocal/Divide Function ÷

## Monadic Use On Floats And Integers

`÷` is a monadic function that returns the `reciprocal` of the argument.

```pometo
÷ 4 5 10
```

```pometo_results
0.25 0.2 0.1
```

## Dyadic Use On Floats And Integers

`÷` is a dyadic function that works in two ways.

If the `rho` of both sides is the same it performs zip-wise `division`

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

## Monadic Use On Complex Nos

For complex numbers the `÷` also calculates the complex reciprocal:

```pometo
÷ 3J4
```

```pometo_results
0.12J¯0.16
```

## Dyadic Use On Complex Nos

This is simple complex division:

```pometo
51j2 ÷ 2j4
```

```pometo_results
5.5J¯10.0
```

## Dyadic Mixed Use On Complex Nos And Plain Numbers

In mixed use the real numbers are cast to a complex number with an imaginary value of zero:

```pometo
4 4J9 ÷ 2J4 2
```

```pometo_results
0.4J¯0.8 2.0J4.5
```