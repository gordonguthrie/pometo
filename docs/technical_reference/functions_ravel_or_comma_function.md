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

## Partial ravelling

If specified with integer indices `ravel` can also operate partially on shapes by ravelling axes using the axis notation. The following command will turn the three dimensional shape into a two dimensional one

```pometo
A ← 2 3 4 ⍴ ⍳ 24
⍴ ,[1 2] A
```

```pometo_results
6 4
```

The range of results can be extended:

```pometo
B ← 2 3 4 5 ⍴ ⍳ 120
⍴ , [2 3 4] B
```

Results in the final three dimensions being collapsed:
```
2 60
```

The first and second axes `2` and `3` have been merged - the size of the array remains unchanged - `60` being `2` times `3` times `4` of course.

## Adding New Dimensions

An additional dimension (with a size of `1` to preserve the no of elements) can be inserted between existing dimensions by using fractional indices:

```pometo
C ← 2 3 4 ⍴ ⍳24
⍴ ,[1.5] C
```

Results in a new shape of:

```
2 1 3 4
```