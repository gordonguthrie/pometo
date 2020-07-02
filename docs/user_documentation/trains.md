# Trains And Function Composition

One of the key elements of `APL` languages is the composition of functions.

# Background

Functions can be applied consequtively. Consider:

```pometo
-,÷10
```

This means (executing right to left):

* `reciprocal`    (`÷`) of `scalar` 10    -> `scalar` 0.1
* `concatenation` (`,`) of `scalar` 0.1   -> `array` [0.1]
* `negation`      (`-`) of `array`  [0.1] -> `array` [¯0.1]

```pometo_results
¯0.1
```

A train, here created by wrapping the functions in brackets, executes differently:

```pometo
(-,÷) 10
```

Results in:

```pometo_results
¯10 0.1
```

The execution sequence is:

* `reciprocal`    (`÷`) of `scalar` 10                   -> `scalar` 0.1
* `negation`      (`-`) of `scalar` 10                   -> `scalar` ¯10
* `concatenation` (`,`) of `scalar` ¯10 and `scalar` 0.1 -> [¯10 0.1]

The technical term for this is a dyadic atop

```apl
(f g h)⍵ → (f ⍵) g (h ⍵)
```

`⍵` in this case is the symbol for the righthand most argument and `f`, `g` and `h` are the functions - in this case `-`, `,` and `÷`.


So we see this translates to:

```apl
(- 5) , (÷ 5)
```

The non-train execution was:

```apl
(- (, (÷ 5))
```

If we have a string of functions bracketed by two vectors they will be applied monadically until the left most one:

```pometo
3-,÷10
```

Results in:

```pometo_results
2.9
```

* MONADIC `reciprocal`    (`÷`) of `scalar` 10                     -> `scalar` 0.1
* MONADIC `concatenation` (`,`) of `array` [0.1]                   -> `array` [0.1]
* DYADIC  `subtraction`   (`-`) of `array` [0.1] from `scalar` 3.0 -> `array` [2.9]


