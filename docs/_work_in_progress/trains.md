# Trains And Function Composition

One of the key elements of `APL` languages is the composition of functions.

## Background

Functions can be applied consequtively. Consider:

```pometo
-,÷10
```

It is `-(,(÷10))`

This means (executing right to left):

| Function     | Sign | Type    | RHS Argument  | Result         |
|--------------|------|---------|---------------|----------------|
|reciprocal    | `÷`  | monadic | `scalar` 10   | `scalar` 0.1   |
|concatenation | `,`  | monadic | `scalar` 0.1  | `array` [0.1]  |
|negation      | `-`  | monadic | `array`  [0.1]| `array` [¯0.1] |

```pometo_results
¯0.1
```

A train, here created by wrapping the functions in brackets, executes differently:

```pometo
(-,÷) 10
```

results in:

```pometo_results
¯10 0.1
```

It resolves to `(-10),(÷10)`

The execution sequence is:

| Function     | Sign | Type    | LHS Argument | RHS Argument | Result            |
|--------------|------|---------|--------------|--------------|-------------------|
|reciprocal    | `÷`  | monadic |              | `scalar` 10  | `scalar` 0.1      |
|negation      | `-`  | monadic |              | `scalar` 10  | `scalar`[¯0.1]    |
|concatenation | `,`  | dyadic  | `scalar` ¯10 | `scalar` 0.1 | `array` [¯10 0.1] |


The technical term for this is a monadic fork.

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

This results in:

```pometo_results
2.9
```

It resolves to `3 - (,(÷10))`:

| Function     | Sign | Type    | LHS Argument   | RHS Argument | Result         |
|--------------|------|---------|----------------|--------------|----------------|
|reciprocal    | `÷`  | monadic |                | `scalar` 10  | `scalar` 0.1   |
|concatenation | `,`  | monadic |                | `scalar` 0.1 | `array` [¯0.1] |
|subtraction   | `-`  | dyadic  | `array` [¯0.1] | `scalar` 3   | `array` [2.9]  |


The creation of trains happens at runtime not compile time. We can see this by looking at variable substitution. As you would expect when a Variable is bound to a function (or an expression that computes to a function) this has the same result as putting `-` directly in the train.


```pometo
A ← -
(A,÷) 10
```

As previously it resolves to `(-10),(÷10)` and results in:

```pometo_results
¯10 0.1
```

But if `A` is replaced by `B` which returns an array we get a different execution order:

```pometo
B ← 2
(B,÷)10
```

The becomes `(2,(÷10))`

```pometo_results
2 0.1
```

It is `(2,(÷10))`

The execution sequence is:

| Function     | Sign | Type    | LHS Argument  | RHS Argument  | Result         |
|--------------|------|---------|---------------|---------------|----------------|
|reciprocal    | `÷`  | monadic |               | `scalar` 10   | `scalar` 0.1   |
|concatenation | `-`  | dyadic  | `scalar` 2.0  | `array`  [0.1]| `array` [2 0.1]|

We see that at runtime in this case the concatenation `,` operator is resolved to be dyadic based on the type of the variable.

Trains can be assigned to variables as functions (trains are functions):

```pometo
MyFun ← -,÷
MyFun 10
```

which returns
```pometo_results
¯10 .01
```

It can also be applied dyadically:

```pometo
MyFun ← -,÷
5 MyFun 10
```

Which gives:

```pometo_results
¯5 0.5
```

This forms a `dyadic fork` (or `dyadic fgh fork`) `((5 - 10),(5 ÷ 10))`

Lets break that execution sequence down:
| Function     | Sign | Type   | LHS Argument | RHS Argument | Result          |
|--------------|------|--------|--------------|--------------|-----------------|
|reciprocal    | `÷`  | dyadic | 'scalar'  5  | `scalar` 10  | `scalar` 0.5    |
|subtraction   | `-`  | dyadic | `scalar`  5  | `scalar` 10  | `scalar` ¯5     |
|concatenation | `,`  | dyadic | `scalar` ¯5  | `scalar` 0.5 | `array` [¯5 0.5]|

## The Six Types Of Trains

Trains resolve 6 ways. The key here is:

* `f` a function
* `g` a function
* `h` a function
* `A` an array
* `⍺` the LHS value to which the train is applied
* `⍵` the RHS value to which the train is applied

```apl
⍺(f g h)⍵ ←→ (⍺ f ⍵) g (⍺ h ⍵)   ⍝ dyadic fork (or dyadic fgh fork)
⍺(A g h)⍵ ←→    A    g (⍺ h ⍵)   ⍝ dyadic fork (or dyadic Agh fork)
⍺(  g h)⍵ ←→         g (⍺ h ⍵)   ⍝ dyadic atop

 (f g h)⍵ ←→ (  f ⍵) g (  h ⍵)   ⍝ monadic fork (or monadic fgh fork)
 (A g h)⍵ ←→    A    g (  h ⍵)   ⍝ monadic fork (or monadic Agh fork)
 (  g h)⍵ ←→         g (  h ⍵)   ⍝ monadic atop
```

