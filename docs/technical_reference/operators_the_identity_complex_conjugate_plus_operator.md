# The Identity/Plus Operator +

## Monadic Use On Floats And Integers

`+` is a monadic operator that for a complex number returns the complex conjugate of the argument, otherwise it is an identity operator

```pometo
+ 0 1 2 ¯1 2.0 3e+1
```

```pometo_results
0 1 2 ¯1 2.0 30.0
```

## Dyadic Use On Floats And Integers

`+` is a dyadic operator that works in two ways.

If the `rho` of both sides is the same it performs zip-wise addition

If either side has a `rho` of [1] it adds the value to every value in the other.

Here are some examples:

```pometo
1 2 3.0 + 3 4 5
```

```pometo_results
4 6 8.0
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

## Monadic Use On Complex Nos

the `+` operator returns the complex conjugate of a complex no.

The complex conjugate of a complex number is the complex number which when multiplied by the original gives a real number.

This is done by flipping the sign of the imaginary component.

```pometo
+ 1J2 1J¯2 1.0J2.0 1.0j¯2.0
```

```pometo_results
1J¯2 1J2 1.0J¯2.0 1.0J2.0
```

## Dyadic Use On Complex Nos

This is normal complex addition - the real parts are summed and the imaginary:

```pometo
1J2 3J¯3 + 4j¯5 ¯1j4
```

```pometo_results
5J¯3 2J1
```

## Dyadic Mixed Use On Complex Nos And Real Numbers

In mixed use the real numbers are cast to a complex number with an imaginary value of zero:

```pometo
1 3J4 + 3J4 1
```

```pometo_results
4J4 4J4
```
