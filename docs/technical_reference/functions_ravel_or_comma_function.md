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
A ← ⍳ 24
B ← 2 3 4 ⍴ A
C ← ,[1 2] B
⍴ C
```

```pometo_results
6 4
```

The range of results can be extended:

```pometo
A ← ⍳ 120
B ← 2 3 4 5 ⍴ A
C ← , [2 3 4] B
⍴ C
```

Results in the final three dimensions being collapsed:
```pometo_results
2 60
```

The first and second axes `2` and `3` have been merged - the size of the array remains unchanged - `60` being `2` times `3` times `4` of course.

***Note***: at the moment the rank can't be expressed as a function that returns a vector.

## Adding New Dimensions

An additional dimension (with a size of `1` to preserve the no of elements) can be inserted between existing dimensions by using fractional indices:

```pometo
C ← 2 3 4 ⍴ ⍳24
⍴ ,[1.5] C
```

Results in a new shape of:

```pometo_results
2 1 3 4
```

## Error Messages

***Note***: Error messages are a bit pish

If the Axis are improperly specified a RANK ERROR will be thrown:

```pometo
C ← 2 3 4 ⍴ ⍳24
⍴ ,[3 4] C
```

Will give:

```pometo_results
Error
C ← 2 3 4 ⍴ ⍳24
⍴ ,[3 4] C
--^
RANK ERROR [Invalid Axis: [3,4] ] on line 2 at character 3
```

This is true for float axis specifications also:

```pometo
C ← 2 3 4 ⍴ ⍳24
⍴ ,[4.5] C
```

Will give:

```pometo_results
Error
C ← 2 3 4 ⍴ ⍳24
⍴ ,[4.5] C
--^
RANK ERROR [Invalid Axis: 4.5 ] on line 2 at character 3
```