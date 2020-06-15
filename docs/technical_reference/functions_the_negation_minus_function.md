# The Negation/Minus Function -

## Monadic Use On Floats And Integers

`-` is a monadic function that returns the `negation` of the argument.

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

## Dyadic Use On Floats And Integers

`-` is a dyadic function that works in two ways.

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

## Monadic Use On Complex Nos

The minus function performs normal sign reversal on both parts of a complex no:

```pometo
- 3J¯5
```

```pometo_results
¯3J5
```

## Dyadic Use On Complex Nos

This is normal complex subraction - both parts are subtracted:

```pometo
1J2 3J¯3 - 4j¯5 ¯1j4
```

```pometo_results
¯3J7 4J¯7
```

## Dyadic Mixed Use On Complex Nos And Real Numbers

In mixed use the real numbers are cast to a complex number with an imaginary value of zero:

```pometo
1 3J4 - 3J4 1
```

```pometo_results
¯2J4 2J4
```
