# Variables

## Variable Naming Conventions

Variables in `pometo` follow the `Erlang` convention - they all start with an uppercase letter and may contain upper and lower case letters, unicode characters (except for `_`, `¯` `∆` and `⍙` and all the punctuation keys you can see on your keyboard) and the digits `0` to `9`.

Like `Erlang` variables they are immutable - once you have bound a variable you cannot change its value.

Variables are bound with the `let` operator ←

```pometo
MyVar99 ← 1 2 3
```

```pometo_results
1 2 3
```

Go crazy with your unicode characters

```pometo
My🧫🎱🧫😍😚💨🍑🧐🐜 ← 1 2.2 3333
```

```pometo_results
1 2.2 3333
```

Variables can be used in place of values in other `Pometo` functions

```pometo
A ← 1 2 3
B ← 4 5 6
A + B
```

```pometo_results
5 7 9
```

If you try and use an unset variable you will get an error in your code

```pometo
MyVar ← 1 2 3
55 66 77 + MYVar
```

```pometo_results
Something happens
```