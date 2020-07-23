# The Direction/Multiply Function `×`

## Monadic Use On Floats And Integers

`×` is a monadic function that returns the signum, or sign of the argument.

When the argument is complex, `×` returns the argument divided by its magnitude.

```pometo
× 0 1 ¯2 3
```

```pometo_results
0 1 ¯1 1
```

## Dyadic Use On Floats And Integers

`×` is a dyadic function that works in two ways.

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

## Monadic Use On Complex Nos

In a real number the monadic `×` gives the direction (ie 1 or ¯1 depending on whether the number is positive or negative). It does the same for complex numbers. It takes a given complex number and rescales it so that is magnitude equals 1. Obviously it is in the complex plane. The magnitude here is the square root of 3 square and 4 squared which equals 5. So we divide each component by 5 to get a complex number pointing in the same direction but with a magnitude of 1:

```pometo
× 3J4
```

```pometo_results
0.6J0.8
```

## Dyadic Use On Complex Nos

This is normal complex number multiplication:

```pometo
2J3 × 4j5
```

```pometo_results
¯7J22
```

## Dyadic Mixed Use On Complex Nos And Real Numbers


This is the same as normal complex multiplication with the real no cast into a imaginary with zero imaginary component:

```pometo
2 × 4j5
```

```pometo_results
8J10
```

```pometo
4j5 × 2
```

```pometo_results
8J10
```


