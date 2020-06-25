# Pometo Data Types

## Numbers (Floats And Integers)

`Pometo` supports both floats and integers as data type. Integers will be coerced into floats in operations like `÷` of two integers or any operation on both an integer and a float

```pometo
1 2 + 3 4
```

```pometo_results
4 6
```

```pometo
1.0 2.0 + 3.0 4.0
```

```pometo_results
4.0 6.0
```


```pometo
1 2.0 + 3 4
```

```pometo_results
4 6.0
```

```pometo
10 ÷ 5
```

```pometo_results
2.0
```

Floats can be written in scientific notation:

```pometo
1.0e-1 + 1.0e+1
```

```pometo_results
10.1
```

Numbers written in scientific notation are always floats, even if written in an integer form:

```pometo
1e+1 + 1
```

```pometo_results
11.0
```

## Complex Nos

Complex numbers are a primitive data type. They are marked with a `j` or `J` (and displayed with `J` only):

The left component is the real, and the right component is the imaginary. The components may be integers or floats and will be type coerced using the same rules as for numbers.

```pometo
1j2 1.0J2.0 4.0e+1J1.0e-1 + 1
```

```pometo_results
2J2 2.0J2.0 41.0J0.1
```