## Functions

There are some gotcha's with the primitive functions.

You need to think about the type that is returned.

Consider:

```pometo
1 + 2
```

This gives

```pometo_results
3
```

And what about this:

```pometo
A ← 1 ⍴ 1
B ← 1 ⍴ 2
A + B
```

Same results, right?

```pometo_results
3
```

Well no, lets see what these return:

```pometo
A ← 1 + 2
⍴ A
```

In this case `1 + 2` returns a scalar.

```pometo_results

```

But in this case

```pometo
A ← 1 ⍴ 1
B ← 1 ⍴ 2
C ← A + B
⍴ C
```

It returns a vector of length 1:

```pometo_results
1
```

Generally if one of the operands is a vector the result is a vector

```pometo
A ← 1 ⍴ 1
B ← A + 2
⍴ B
```

this returns a vector of length 1:

```pometo_results
1
```

```pometo
B ← 1 ⍴ 2
C ← 1 + B
⍴ C
```

and this also returns a vector of length 1:

```pometo_results
1
```
