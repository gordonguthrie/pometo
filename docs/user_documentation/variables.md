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