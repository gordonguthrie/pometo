# Rank

Consider this function:

```apl
A ← 2 3 4 ⍴ ⍳ 24
```

`A` look like:

```apl
 1  2  3  4
 5  6  7  8
 9 10 11 12

13 14 15 16
17 18 19 20
21 22 23 24
```

And has the shape:

```apl
2 3 4
```

If we then run
```apl
2 / A
```

It gives:

```apl
 1  1  2  2  3  3  4  4
 5  5  6  6  7  7  8  8
 9  9 10 10 11 11 12 12

13 13 14 14 15 15 16 16
17 17 18 18 19 19 20 20
21 21 22 22 23 23 24 24
```

The shape now looks like:
```apl
2 3 8
```

`/` has been applied on the last axis

On the first axis:

```apl
      2/[1] A
```

```apl
 1  2  3  4
 5  6  7  8
 9 10 11 12

 1  2  3  4
 5  6  7  8
 9 10 11 12

13 14 15 16
17 18 19 20
21 22 23 24

13 14 15 16
17 18 19 20
21 22 23 24
```

with shape:

```apl
4 3 4
```

On the 2nd axis:

```apl
2/[2] A
```

gives
```apl
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

with shape of:

```apl
2 6 4
```

and the 3rd axis is the same as the last/default

```apl
2/[3] A
```

giving

```apl
 1  1  2  2  3  3  4  4
 5  5  6  6  7  7  8  8
 9  9 10 10 11 11 12 12

13 13 14 14 15 15 16 16
17 17 18 18 19 19 20 20
21 21 22 22 23 23 24 24
```

We can have a vector on the LHS if it matches:

```apl
2 3 /[1] A
```

Gives

```apl
 1  2  3  4
 5  6  7  8
 9 10 11 12

 1  2  3  4
 5  6  7  8
 9 10 11 12

13 14 15 16
17 18 19 20
21 22 23 24

13 14 15 16
17 18 19 20
21 22 23 24

13 14 15 16
17 18 19 20
21 22 23 24
```

This has a shape of

```apl
5 3 4
```

The first element was repeated twice, the second three times

[2 3] is the prefix of the shape [2 3 4]

```apl
C ← 1 2 3 4 ⍴ ⍳ 24
```

looks like

```apl
 1  2  3  4
 5  6  7  8
 9 10 11 12

13 14 15 16
17 18 19 20
21 22 23 24
```

```apl
1 2 3 /[3] C
```

gives

```apl
 1  2  3  4
 5  6  7  8
 5  6  7  8
 9 10 11 12
 9 10 11 12
 9 10 11 12

13 14 15 16
17 18 19 20
17 18 19 20
21 22 23 24
21 22 23 24
21 22 23 24
```

[1 2 3] is the prefix of [1 2 3 4] with 4 removed

and we can repeat this with

```apl
1 2  /[2] C
```

```apl
 1  2  3  4
 5  6  7  8
 9 10 11 12

13 14 15 16
17 18 19 20
21 22 23 24

13 14 15 16
17 18 19 20
21 22 23 24
```
