1J¯1 3J¯4# Smoke Tests

Because the documentation is also the test suite it is sometimes necessary to just bang out primitive smoke tests that would clutter the user-facing documentation.

This section is one such. It is a test suite for basic execution paths in `pometo_runtime`.

Any test failure arising from this documentation will cause a slew of errors across the test suite.


## Unitary Negation

```pometo
- 1
```

```pometo_results
¯1
```

```pometo
- 1J3
```

```pometo_results
¯1J¯3
```

## Monadic Branches - Real Numbers

### Scalar first

```pometo
+ 1
```

```pometo_results
1
```

### Now Vectors

```pometo
+ 1 2
```

```pometo_results
1 2
```

```pometo
- 1 2
```

```pometo_results
¯1 ¯2
```

```pometo
× 1 ¯2
```

```pometo_results
1 ¯1
```

```pometo
÷ 1 2 ¯4
```

```pometo_results
1.0 0.5 ¯0.25
```

## Monadic Branches - Complex Numbers

This covers mixed vectors and scalars

```pometo
+ 1j2 2J¯1
```

```pometo_results
1J¯2 2J1
```

```pometo
- 1j2 2J¯1
```

```pometo_results
¯1J¯2 ¯2J1
```

```pometo
× 3J4
```

```pometo_results
0.6J0.8
```

```pometo
÷ 3J4
```

```pometo_results
0.12J¯0.16
```

## Dyadic Branches - Real Numbers

### Scalars first

```pometo
1 + 2
```

```pometo_results
3
```

### Now Mixed

```pometo
1 + 2 3
```

```pometo_results
3 4
```

```pometo
2 3 + 1
```

```pometo_results
3 4
```


### Now Pure Vectors

```pometo
4 5 + 1 2
```

```pometo_results
5 7
```

```pometo
2 3 - 1 2
```

```pometo_results
1 1
```

```pometo
4 5 × 1 2
```

```pometo_results
4 10
```

```pometo
10 10 ÷ 1 2
```

```pometo_results
10.0 5.0
```

## Dyadic Branches - Complex Numbers

```pometo
4J2 5J¯1 + 3J3 2J5
```

```pometo_results
7J5 7J4
```

```pometo
4J2 5J1 - 3J3 2J5
```

```pometo_results
1J¯1 3J¯4
```

```pometo
2J3 × 4j5
```

```pometo_results
¯7J22
```

```pometo
51j2 ÷ 2j4
```

```pometo_results
5.5J¯10.0
```

## Dyadic Branches - Mixed Real And Complex Numbers

```pometo
4J2 5 + 3 2J5
```
```pometo_results
7J2 7J5
```

```pometo
4J2 5 - 3 2J5
```

```pometo_results
1J2 3J¯5
```

```pometo
4J2 5 × 3 2J5
```

```pometo_results
12J6 10J25
```

```pometo
4J2 10 ÷ 2 2J5
```

```pometo_results
2.0J1.0 0.6896551724137931J¯1.7241379310344827
```

## Nested Arrays

```pometo
1 2 (3 4)
```

```pometo_results
1 2  3 4
```