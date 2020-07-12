# The Replicate Functions `/` and `⌿`

## Hybridity

`/` and `⌿` are hybrids - they are both functions and operators. For use of `/` and `⌿` as operators please see the appropriate [operator documentation](./operators_forwardslash_or_the_reduce_operator.md).

## '/' Used Dyadically With Constants On Both Sides

When used dyadically with constants the LHS must be a number or boolean scalar or a scalar array containing only numbers or booleans.

The right ha

```pometo
3 4 / 5 6
```

it creates a vector with `3` copies of `5` and `4` copies of `6`

```pometo_results
5 5 5 6 6 6 6
```

This can take nested vectors on the RHS of course:

```pometo
3 4 / (2 3) (4 (5 6))
```
giving:

```pometo_results
┌───┐ ┌───┐ ┌───┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐
│2 3│ │2 3│ │2 3│ │4 ┌───┐│ │4 ┌───┐│ │4 ┌───┐│ │4 ┌───┐│
└───┘ └───┘ └───┘ │  │5 6││ │  │5 6││ │  │5 6││ │  │5 6││
                  │  └───┘│ │  └───┘│ │  └───┘│ │  └───┘│
                  └───────┘ └───────┘ └───────┘ └───────┘
```

But not on the LHS.

## Default Rank With Scalars on the LHS

`/` operates on the first axis. Remember that axes are numbered right to left so the first axis here has a length `3`

```pometo
A ← 2 3 ⍴ 1 2 3 4 5 6
2 / A
```

Gives:

```pometo_results
1 1 2 2 3 3
4 4 5 5 6 6
```

## Default Rank With Vectors on the LHS

If the length of the LHS vector is the same as the principle dimension then the elements will be replicated pairswise:

```pometo
A ← 2 3 ⍴ 1 2 3 4 5 6
2 3 4 / A
```

results in:

```pometo_results
1 1 2 2 2 3 3 3 3
4 4 5 5 5 6 6 6 6
```

## The Barred Replicate Function `⌿`  With Scalars on the LHS

This function applies to the last axis:

```pometo
A ← 2 3 ⍴ 1 2 3 4 5 6
2 3 ⌿ A
```

Gives
```pometo_results
1 2 3
1 2 3
4 5 6
4 5 6
4 5 6
```

The same caveats pertain with regard the length of the LHS - except this must match the last dimension.

## Use With Axes  With Scalars on the LHS

Both `/` and `⌿` can be used with axis notation:

```pometo
A ← ⍳ 24
B ← 2 3 4 ⍴ A
2 ⌿[2] B
```

```pometo_results
1  2  3  4
1  2  3  4
5  6  7  8
5  6  7  8
9 10 11 12
9 10 11 12

13 14 15 16
13 14 15 16
17 18 19 20
17 18 19 20
21 22 23 24
21 22 23 24
```

When used with axis notation the symbols are intechangable:

```pometo
A ← 2 3 4 ⍴ 1 2 3 4 5 6
2 /[2] A
```

```pometo_results
1 2 3 4
1 2 3 4
5 6 1 2
5 6 1 2
3 4 5 6
3 4 5 6

1 2 3 4
1 2 3 4
5 6 1 2
5 6 1 2
3 4 5 6
3 4 5 6
```
`/` with out axis simply means ***replicate along the principal axis*** and `⌿` means ***replicate on the lowest axis***.

## With Vectors On The LHS

```pometo
A ← ⍳ 24
B ← 1 2 3 4 ⍴ A
2 1 /[2] B
```

The first block of [3, 4] is duplicated and the second is simply replicated.

```pometo_results
1  2  3  4
5  6  7  8
9 10 11 12

1  2  3  4
5  6  7  8
9 10 11 12

13 14 15 16
17 18 19 20
21 22 23 24
```

The vector `[2 1]` has two elements and the second axis is `2`

## Generating LENGTH ERRORs

If the LHS doesn't have as many elements as the axis a `LENGTH ERROR` will be triggered:

```pometo
A ← ⍳ 24
B ← 2 3 4 ⍴ A
2 2/[2] B
```

```pometo_results
Error
A ← ⍳ 24
B ← 2 3 4 ⍴ A
2 2/[2] B
---^
LENGTH ERROR (LHS vector doesn't match the selection axis:LHS has 2 elements - RHS Axis has 3
) on line 3 at character 4
```

## Invalid Axes and INDEX ERRORs

Trying to specify an Axis that is out of bounds will trigger an `INDEX ERROR`.

```pometo
A ← ⍳ 24
B ← 1 2 3 ⍴ A
2 2 3/[2] B
```

throws this error:

```pometo_results
Error
A ← ⍳ 24
B ← 1 2 3 ⍴ A
2 2 3/[2] B
-----^
LENGTH ERROR (LHS vector doesn't match the selection axis:LHS has 3 elements - RHS Axis has 2
) on line 3 at character 6
```

## LHS Must Be A Scalar Or A Vector Or You Get A DOMAIN ERROR

If the LHS is an array a `DOMAIN ERROR` will be thrown:

```pometo
A ← ⍳ 24
B ← ⍳ 4
C ← 2 2 ⍴ A
D ← 2 2 ⍴ B
D /[2] C
```

```pometo_results
Error
A ← ⍳ 24
B ← ⍳ 4
C ← 2 2 ⍴ A
D ← 2 2 ⍴ B
D /[2] C
--^
DOMAIN ERROR (LHS must be a vector or a scalar:It has a shape of [2,2]) on line 5 at character 3
```

## Implementation Notes

The `reduce` operators `/` and `⌿' always convert `lazy` vectors to `eager` before processing.
