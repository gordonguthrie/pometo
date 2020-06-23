# The Shape Function ⍴

## Monadic Use On Scalars

When used monadically `⍴` returns the shape of an object. If the object is a scalar the result is a blank line

```pometo
⍴ 3
```

```pometo_results

```

## Monadic Use On Arrays

If the object has shape, if it is an array, `r` returns the shape:

```pometo
⍴ 3 4 5
```

```pometo_results
3
```

It works on all scalars

```pometo
⍴ 1J2 3J4
```

```pometo_results
2
```

## Dyadic Use On Scalars

Dyadically `⍴` is used to reshape an object. If necessary it will create an array of the appropriate shape and populate it with the scalar:

```pometo
2 3 ⍴ 4
```

```pometo_results
4 4 4
4 4 4
```

This extends to higher dimensions:

```pometo
2 3 4 ⍴ 4
```

```pometo_results
4 4 4 4
4 4 4 4
4 4 4 4

4 4 4 4
4 4 4 4
4 4 4 4
```

## Dyadic Use On Arrays

Dyadically `⍴` is used to reshape an object. It will stripe an existing vector over its space:

```pometo
2 3 4 ⍴ 9 8 7
```

```pometo_results
9 8 7 9
8 7 9 8
7 9 8 7

9 8 7 9
8 7 9 8
7 9 8 7
```

It also works on arrays of arrays:

```pometo
A ← 2 ⍴ 9
A ← 2 ⍴ 9
B ← 2 ⍴ 99
2 3 ⍴ A B
```

```pometo_results
┌─────┬─────┬─────┐
│9 9  │99 99│9 9  │
├─────┼─────┼─────┤
│99 99│9 9  │99 99│
└─────┴─────┴─────┘
```

The left hand side must be an array of integers:

```pometo
2.2 3 4 ⍴ 9 8 7
```

```pometo_results
ERROR
DOMAIN ERROR
```

The formatter likes to keep things neat, which is nice:

```pometo
3 4⍴ 9999 8 7
```

with some cheeky alignments going on:

```pometo_results
9999    8    7 9999
   8    7 9999    8
   7 9999    8    7
```

`⍴` is a procrustean operator - if there aren't enough elements on the right hand side it will generate them:

```pometo
2 3 ⍴ 9 8 7 6
```
giving:

```pometo_results
9 8 7
6 9 8
```
If there are too many it will chop some off:

```pometo
1 2 ⍴ 4 4 4
```

giving:

```pometo_results
4 4
```
