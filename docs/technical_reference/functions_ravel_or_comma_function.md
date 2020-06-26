# The ravel function `,`

## Ravel is a monadic function

Ravel (a reverse-coinage from ***unravel***) turns an object of arbitrary shape back into a vector.

```pometo
A ← 2 2 ⍴ 1 22 333 4444
, A
```

Gives a vector not a matrix:

```pometo_results
1 22 333 4444
```

If applied to a scalar `,` converts it to a vector of length 1

```pometo
A ← , 1
⍴ A
```

gives

```pometo_results
1
```