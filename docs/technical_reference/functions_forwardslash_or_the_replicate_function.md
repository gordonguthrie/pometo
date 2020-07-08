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

## Default Rank

`/` operates on the first axis:

```pometo
2 / 2 3 ⍴ 1 2 3 4 5 6
```

Gives:

```pometo_results
1 1 2 2 3 3
4 4 5 5 6 6
```

If the length of the LHS vector is the same as the principle dimension then the elements will be replicated pairswise:

```pometo
2 3 4 / 2 3 ⍴ 1 2 3 4 5 6
```

results in:

```pometo_results
1 1 2 2 2 3 3 3 3
4 4 5 5 5 6 6 6 6
```

## The Barred Replicate Function `⌿`

This function applies to the last axis:

```pometo
2 3 ⌿ 2 3 ⍴ 1 2 3 4 5 6
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

## Use With Axes

Both `/` and `⌿` can be used with axis notation:

```pometo
2 ⌿[2] 2 3 4  ⍴ 1 2 3 4 5 6
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

When used with axis notation the symbols are intechangable:

```pometo
2 /[2] 2 3 4  ⍴ 1 2 3 4 5 6
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

