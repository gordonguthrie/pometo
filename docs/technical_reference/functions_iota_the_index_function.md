# The Index Function ⍳

## Monadic Use On Integers

TESTS ON THIS DISABLED AS IOTA IS NOT IMPLEMENTED YET

`⍳` is a monadic function which when applied to a vector of integers generates a matrix of indices.

```xxpometo
⍳3
```

```xxpometo_results
1 2 3
```

It is multidimensional, of course:

```xxpometo
⍳3 2 1
```

```xxpometo_results
1 1 1
1 2 1

2 1 1
2 2 1

3 1 1
3 2 1
```

## Use With Other Data Types

Attemps to use `i` on a vector of other values will throw a `DOMAIN ERROR`:

```xxpometo
⍳ 3 2 1.0
```

```xxpometo_results
Error DOMAIN ERROR
```