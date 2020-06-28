# The Index Function `⍳`

## DEVELOPERS NOTE

For the moment `⍳` is returning an unindexed array - this might need to change.

## Monadic Use On Integers Scalars

`⍳` is a monadic function which when applied to a vector of integers or a scalar generates a matrix of indices.

```pometo
⍳ 2
```

```pometo_results
1 2
```

## Monadic Use On Boolean Scalars

A boolean scalar will be cast to an integer scalar

```pometo
⍳ 1
```

```pometo_results
1
```

## Monadic Use On Integers Vectors

It is multidimensional, of course:

```pometo
⍳3 2 1
```

```pometo_results
┌─────┐
│1 1 1│
└─────┘
┌─────┐
│2 1 1│
└─────┘

┌─────┐
│3 1 1│
└─────┘
┌─────┐
│1 2 1│
└─────┘

┌─────┐
│2 2 1│
└─────┘
┌─────┐
│3 2 1│
└─────┘
```

## Use With Other Data Types

If applied to a boolean scalar `⍳` will return a degenerate multidimensional scalar of type array with a null argument if any of the elements are set to zero.

```pometo
⍳ 1 1 1 0
```

```pometo_results

```

It is degenerated because it keeps its dimensionality (but the presence of a single `0` causes it to collapse:

```pometo
A ← ⍳ 1 1 1 0
⍴ A
```

```pometo_results
1 1 1 0
```

However if all the elements are 1 it will return a degenerate multidimensional array of type array with the value `1`

```pometo
⍳ 1 1 1 1
```

```pometo_results
┌───────┐
│1 1 1 1│
└───────┘
```

Attemps to use `⍳` on a vector of other values will throw a `DOMAIN ERROR`:

```pometo
⍳ 3 2 1.1
```

giving:

```pometo_results
Error
⍳ 3 2 1.1
--^
DOMAIN ERROR (⍳ only accepts integer arguments and was called with:[3,2,1.1]) on line 1 at character 3
```

Attempts to use `⍳` on a scalar float will also throw an error (even if the float evaluates to an integer):

```pometo
⍳ 1.0
```
giving:


```pometo_results
Error
⍳ 1.0
--^
DOMAIN ERROR (⍳ only accepts integer arguments and was called with:1.0) on line 1 at character 3
```

