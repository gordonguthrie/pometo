# Complex Numbers

This section covers both the parsing problems that complex numbers create and the solution that we use to resolve it.

## The J Clash

In Pometo variables must begin with an uppercase letter - and complex numbers can be expressed with either a `j` or a `J`.

It is consquently hard to lex complex numbers and variables and some of these issues must be resolved at parse time.

Consider the following code example:

```pometo
JAY12ab ← 1 2 3
JAY12ab + 1
```

This must resolve correctly to a variable called `JAY12ab` which takes the value:

```pometo_results
2 3 4
```

The lexer will return two tokens `J` and `AY12ab` where the second is a valid variable name.

However there are more challenging parsing issues, consider:

```pometo
J12ab ← 1 2 3
J12ab + 1
```

In this case the fragment after the `J` (`12ab`) doesn't represent a valid variable name.

But this still must resolve correctly to a variable called `J12ab` which takes the value:

```pometo_results
2 3 4
```

Or this case

```pometo
J12 ← 1 2 3
J12 + 1
```

In this case the fragment after the `J` is a valid integer.

But this still must resolve correctly to a variable called `J12` which takes the value:


```pometo_results
2 3 4
```

We also need to handle a set of edge cases including variables that are not used:

```pometo
_JjJjJ12ab ← 1 2 3
_JjJjJ12ab + 1
```

```pometo_results
2 3 4
```

But the following should throw an error:
```pometo
_jjJjJ12ab ← 1 2 3
```

```pometo_results
Error
_jjJjJ12ab ← 1 2 3
^
SYNTAX ERROR (syntax error before: :{maybe_varfrag,1,\"_\",\"_\"}) on line 1 at character 1
```

And cases where variable names include what would otherwise be valid complex numbers:

```pometo
My2J4Var ← 1 2 3
My2J4Var + 1
```

```pometo_results
2 3 4
```


```pometo
My2j4Var ← 1 2 3
My2j4Var + 1
```

```pometo_results
2 3 4
```

Proper imaginary numbers are shown below:

```pometo
A ← 1j2 ¯3.0J4.0 5.0e-4J¯5.4e1
A + 1
```

Needs to resolve to:

```pometo_results
2J2 ¯2.0J4.0 1.0005J¯54.0
```

And, of course, combinations of variable names that contain possible complex numbers need to co-exist with actual complex numbers:


```pometo
My2J4Var ← 1j2 ¯3.0J4.0 5.0e-4J¯5.4e1
My2J4Var + 1
```

giving:

```pometo_results
2J2 ¯2.0J4.0 1.0005J¯54.0
```

Note that while both `j` and `J` are accepted as inputs only `J` is output.

## Solving the parsing problem

The easiest way to solve these problems is via a two step lexing. We lex the input into fragments which include `maybe_imaginary` and `maybe_varfrag` elements.

We first to a simple look-ahead parser to extract all the complex numbers.

Then we process the token stream to merge all the mergable fragments into variables.

We do this with the unstripped token stream that has whitespace tokens in it - tokens that we then remove for the input to the parser.