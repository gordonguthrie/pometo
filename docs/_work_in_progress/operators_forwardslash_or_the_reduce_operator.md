# The Reduce Operatord `/` and `⌿`

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


## Dyadic Use On Scalars Are An Error

Attempting to use '/' dyadically on a scalar produces an error:

```pometo
1 +/ 1
```

Resulting in:

```pometo_results
Error
RANK ERROR
1 +/ 1
^
```

## Dyadic Use On Vectors

The '\' vector when used dyadically applies a sliding window:

```pometo
2 +/ 1 2 3 4
```

Giving:

```pometo_results
3 5 7
```

This is `1 + 2` then `2 + 3` then `3 + 4`

This scales up to objects of higher shape

```pometo
2 +/ 3 3 ⍴ 1 2 3 4
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

which is, of course:

```apl_results
1 2 3
4 1 2
3 4 1
```

