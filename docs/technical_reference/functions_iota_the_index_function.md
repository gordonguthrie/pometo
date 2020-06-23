# The Index Function ⍳

## Monadic Use On Integers

`⍳` is a monadic function which when applied to a vector of integers generates a matrix of indices.

```pometo
⍳3
```

```pometo_results
1 2 3
```

It is multidimensional, of course:

```pometo
⍳3 2 1
```

```pometo_results
1 1 1
1 2 1

2 1 1
2 2 1

3 1 1
3 2 1
```

## Use With Other Data Types

Attemps to use `i` on a vector of other values will throw a `DOMAIN ERROR`:

```pometo
⍳ 3 2 1.0
```

```pometo_results
Error DOMAIN ERROR
```