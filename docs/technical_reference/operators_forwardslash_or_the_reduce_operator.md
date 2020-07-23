# The Reduce Operator `/` and `⌿`

## Hybridity

`/` and `⌿` are hybrids - they are both functions and operators. For use of `/` and `⌿` as functions please see the appropriate [operator documentation](./functions_forwardslash_or_the_replicate_function.md).

When used as operators `/` and `⌿` apply functions across the arguments of a vector. If applied to a scalar they first convert it to a vector with a single element.

## Monadic Use On Scalars

The '/' operator applys a function to a scalar:

```pometo
+/1
```

```pometo_results
1
```

It returns a scalar.

```pometo
⍴ +/1
```

Gives:

```pometo_results

```

## Monadic Use On Vectors

The '/' operator successively applies a function to the elements of a vector:

```pometo
+/ 1 2 3 4
```

```pometo_results
10
```

and

```pometo
×/1 2 3 4
```

giving:

```pometo_results
24
```

'/' always reduces the rank of the operand to the right (hence the name reduce):

```pometo
A ← 2 2 ⍴ 1 22 333
+/ A
```

gives:

```pometo_results
23 334
```

`A` of course was a 2 by 2 matrix:

```apl_results
  1 22
333  1
```

Each dimensions was reduced seperately.

## Gotcha's

The `/` operator reduces the rank and it will mondically reduce a vector of length 1 to a scalar:

```pometo
A ← 1 ⍴ 4
⍴ +/ A
```

Gives a null result - its a scalar:

```pometo_results

```

## Dyadic Use On Scalars Are An Error

Attempting to use '/' dyadically on a scalar produces an error:

```pometo
1 +/ 1
```

Resulting in:

```pometo_results
Error
1 +/ 1
-----^
RANK ERROR (RHS must be an array:It is a scalar) on line 1 at character 6
```

It works on a vector of length 1, of course:

```pometo
A ← 1 ⍴ 4
1 +/ A
```

Resulting in:

```pometo_results
4
```

## Moar Gotcha's

See the points for improvement page for outstanding issues.

## Dyadic Use On RHS Vectors With An LHS Scalar

The '/' vector when used dyadically applies a sliding window:

```pometo
2 +/ 1 2 3 4
```

Giving:

```pometo_results
3 5 7
```

The LHS scalar can be replaced with a vector of length 1 with the same results:

```pometo
A ← 1 ⍴ 2
A +/ 1 2 3 4
```

and as expected:

```pometo_results
3 5 7
```

This is `1 + 2` then `2 + 3` then `3 + 4`

This scales up to objects of higher shape

```pometo
A ← 3 3 ⍴ 1 2 3 4
2 +/ A
```

Which returns:

```pometo_results
3 5
5 3
7 5
```
We can derive that by looking at the intermediate shape:

```apl
3 3 ⍴ 1 2 3 4
```

which is of course:

```apl_results
1 2 3
4 1 2
3 4 1
```

The shape has been reduced from `3 3` to `3 2`

## Dyadic Use On RHS Vectors With An LHS Vector

This throws a LENGTH ERROR:

```pometo
2 2 +/ 1 2 3 4
```

Giving

```pometo_results
Error
2 2 +/ 1 2 3 4
----^
LENGTH ERROR (Reduction window is too long for the axis:LHS has window size of [2,2] elements - RHS Axis has 4
) on line 1 at character 5
```

## Reducing With Ranks

By default `/` operates on the first (ie right hand axis) but you can specify the rank you wish to reduce on:

```pometo
A ← ⍳ 24
B ← 2 3 4 ⍴ A
2 +/[2] B
```

Gives the sum over the second rank:

```pometo_results
 6  8 10 12
14 16 18 20

30 32 34 36
38 40 42 44
```

The value of `A` is:

```apl_results
1  2  3  4
5  6  7  8
9 10 11 12
```

and a shape of `3 4`. After reduction with rank its shape is `2 4`

## The Barred Reduce Operator ⌿

Like the function `⌿` the `barred reduce` operator operates on the last axis:

Whereas `/` without a rank:

```pometo
A ← 2 2 ⍴ 1 22 333 444
+/ A
```

gives:

```pometo_results
23 777
```

With `⌿`:

```pometo
A ← 2 2 ⍴ 1 22 333 444
+⌿ A
```

you get:

```pometo_results
334 466
```

As in the function case if you explicity specify a rank then `/` and `⌿` behave identically.
