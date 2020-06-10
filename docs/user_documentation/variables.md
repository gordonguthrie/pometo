# Variables

## Variable Naming Conventions

Variables in `pometo` follow the `Erlang` convention - they all start with an uppercase letter and may contain upper and lower case letters, the digits `0` to `9` and `_` and `@`.

Like `Erlang` variables they are immutable - once you have bound a variable you cannot change its value.

Variables are bound with the `let` operator `←`. (Note in this example we are prefixing the variable with `_` the ***I am not being used*** prefix for operators. You don't need this in the interpreter but if you write `Pometo` modules they won't compile - you will get an error.

```pometo
_MyVar99 ← 1 2 3
```

```pometo_results
1 2 3
```

Variables can be used in place of values in other `Pometo` functions:

```pometo
A ← 1 2 3
B ← 4 5 6
A + B
```

```pometo_results
5 7 9
```

And of course they can be mixed and matched:

```pometo
A ← 1 2 3
B ← 5 6 7
A + 4 5 6
1 2 3 - B
```

```pometo_results
¯4 ¯4 ¯4
```

```pometo
A ← 1 2 3
B ← 5 6 7
C ← A
D ← B
C ÷ D
```

```pometo_results
0.2 0.3333333333333333 0.42857142857142855
```

If you try and use an unset variable you will get an error in your code

```pometo
MyVar ← 1 2 3
55 66 77 + MYVar
```

```pometo_results
Error
55 66 77 + MYVar
-----------^
VARIABLE NOT DEFINED (MYVar:variable is not defined) on line 2 at character 12
```