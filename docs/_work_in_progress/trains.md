# Trains And Function Composition

One of the key elements of `APL` languages is the composition of functions.

## Background

Functions can be applied consequtively. Consider:

```pometo
-+÷10
```

It is `-(,(÷10))`

This means (executing right to left):

| Function     | Sign | Type    | RHS Argument  | Result        |
|--------------|------|---------|---------------|---------------|
|reciprocal    | `÷`  | monadic | `scalar` 10   | `scalar`  0.1 |
|plus          | `+`  | monadic | `scalar`  0.1 | `scalar`  0.1 |
|negation      | `-`  | monadic | `scalar`  0.1 | `scalar` ¯0.1 |

```pometo_results
¯0.1
```

A train, here created by wrapping the functions in brackets, executes differently:

```pometo
(-+÷) 10
```

results in:

```pometo_results
¯9.9
```

It resolves to `(-10),(÷10)`

The execution sequence is:

| Function     | Sign | Type    | LHS Argument | RHS Argument  | Result        |
|--------------|------|---------|--------------|---------------|---------------|
|reciprocal    | `÷`  | monadic |              | `scalar` 10   | `scalar`  0.1 |
|negation      | `-`  | monadic |              | `scalar` 10   | `scalar` ¯0.1 |
|plus          | `+`  | dyadic  | `scalar` ¯10 | `scalar`  0.1 | `scalar` ¯9.9 |


The technical term for this is a monadic fork.

```apl
(f g h)⍵ → (f ⍵) g (h ⍵)
```

`⍵` in this case is the symbol for the righthand most argument and `f`, `g` and `h` are the functions - in this case `-`, `,` and `÷`.


So we see this translates to:

```apl
(- 10),(÷ 10)
```

The non-train execution was:

```apl
(-(,(÷ 10))
```

If we have a string of functions bracketed by two vectors they will be applied monadically until the left most one:

```pometo
3-+÷10
```

This results in:

```pometo_results
2.9
```

It resolves to `3 - (+(÷10))`:

| Function     | Sign | Type    | LHS Argument | RHS Argument  | Result       |
|--------------|------|---------|--------------|---------------|--------------|
|reciprocal    | `÷`  | monadic |              | `scalar` 10   | `scalar` 0.1 |
|plus          | `+`  | monadic |              | `scalar`  0.1 | `scalar` 0.1 |
|subtraction   | `-`  | dyadic  | `scalar` 3   | `scalar` ¯0.1 | `scalar` 2.9 |

## The Six Types Of Trains

Trains resolve 6 ways. The key here is:

* `f` a function
* `g` a function
* `h` a function
* `A` an array
* `⍺` the LHS value to which the train is applied
* `⍵` the RHS value to which the train is applied

```apl
 (f g h)⍵ ←→ (  f ⍵) g (  h ⍵)   ⍝ monadic fork (or monadic fgh fork)
 (A g h)⍵ ←→    A    g (  h ⍵)   ⍝ monadic fork (or monadic Agh fork)
 (  g h)⍵ ←→         g (  h ⍵)   ⍝ monadic atop

⍺(f g h)⍵ ←→ (⍺ f ⍵) g (⍺ h ⍵)   ⍝ dyadic fork (or dyadic fgh fork)
⍺(A g h)⍵ ←→    A    g (⍺ h ⍵)   ⍝ dyadic fork (or dyadic Agh fork)
⍺(  g h)⍵ ←→         g (⍺ h ⍵)   ⍝ dyadic atop
```


## Monadic fgh fork

The creation of trains happens at runtime not compile time. We can see this by looking at variable substitution. As you would expect when a Variable is bound to a function (or an expression that computes to a function) this has the same result as putting `-` directly in the train.

```pometo
A ← -
(A+÷) 10
```

As previously it resolves to `(-10)+(÷10)` and results in:

```pometo_results
¯9.9
```

Trains can be assigned to variables as functions (trains are functions):

```pometo
MyFun ← -+÷
MyFun 10
```

which returns
```pometo_results
¯9.9
```

## Monadic Agh fork

But if `A` of the first `monadic fgh fork` is replaced by `B` which returns an `scalar` we get a different execution order - and a `monadic Agh fork`:

```pometo
B ← 2
(B+÷)10
```

This becomes `(2(+÷10))`:

```pometo_results
2.1
```

The execution sequence is:

| Function     | Sign | Type    | LHS Argument | RHS Argument | Result       |
|--------------|------|---------|--------------|--------------|--------------|
|reciprocal    | `÷`  | monadic |              | `scalar` 10  | `scalar` 0.1 |
|plus          | `+`  | dyadic  | `scalar` 2.0 | `scalar` 0.1 | `scalar` 2.1 |

We see that at runtime in this case the concatenation `+` operator is resolved to be dyadic based on the type of the variable.

## Monadic atop

A `monadic atop` just has less functions

```pometo
-÷10
```

Which results in:

```pometo_results
¯0.1
```

The execution sequence is:

| Function     | Sign | Type    | RHS Argument | Result        |
|--------------|------|---------|--------------|---------------|
|reciprocal    | `÷`  | monadic | `scalar` 10  | `scalar` 0.1  |
|minus         | `-`  | dyadic  | `scalar` 0.1 | `scalar` ¯0.1 |


## Dyadic fgh fork


`dyadic fgh fork`s are easy:

```pometo
A ← -
B A+÷
5 B 10
```

Which gives:

```pometo_results
¯4.5
```

This forms a `dyadic fork` (or `dyadic fgh fork`) `((5 - 10),(5 ÷ 10))`

Lets break that execution sequence down:

| Function     | Sign | Type   | LHS Argument | RHS Argument | Result        |
|--------------|------|--------|--------------|--------------|---------------|
|reciprocal    | `÷`  | dyadic | 'scalar'  5  | `scalar` 10  | `scalar`  0.5 |
|subtraction   | `-`  | dyadic | `scalar`  5  | `scalar` 10  | `scalar` ¯5   |
|concatenation | `,`  | dyadic | `scalar` ¯5  | `scalar` 0.5 | `scalar` ¯4.5 |


## Dyadic Agh fork

We can create a `dyadic Agh fork` by reassigning `A`.

```pometo
A ← 3
B A+÷
5 B 10
```

Which gives:

```pometo_results
¯3.5
```

## Dyadic atop

```pometo
A ← -÷
5 A 10
```

```pometo_results
4.9
```

## Trains Of Trains

A train does not have to be 2 or three functions long - it can grow continously. With an odd number of functions the final product is a `fork` with an even it is an `atop`:

```apl
e f g h i j k → e f(g h(i j k))     ⍝ fork(fork(fork))
f g h i j k →   f(g h(i j k))       ⍝ atop(fork(fork))
```

For a monadic case the right argument `⍵` is stripped through the execution, and for the `dyadic` case both the left `⍺` and the right `⍵` ones are:

Lets work through some examples:

## Monadic Atop Train Of Trains

```pometo
(÷-×⍴) 2 4
```

This is the equivalent of:

```apl
÷ ((- 2 4) × (⍴ 2 4))
```

(if you try and work these steps out manually remember the

which gives:
```pometo_results
¯0.25 ¯0.125
```

## Dyadic Atop Train Of Trains

We can see that from:

```pometo
1 4 (÷-+×) 3 2
```

Which results in:

```pometo_results
1 0.1
```

This calculation could be exanded with brackets to read:

```pometo
÷ ((1 4 - 3 2) + (1 4 × 3 2))
```

giving the same result:

```pometo_results
1 0.1
```

## Monadic fgh Fork Train Of Trains

Lets add another operator:

```pometo
(÷-×⍴÷) 2 4
```

This is the equivalent of:

```apl
((÷ 2 4) - ((x 2 4) ⍴ (÷ 2 4)))
```

which gives:
```pometo_results
0.0 ¯0.25
```

## Dyadic fgh Fork Train Of Trains

Lets add another operator:

```pometo
2 4 (÷⍴-×÷) 1 2
```

This is the equivalent of:

```apl
((2 4 ÷ 1 2) ⍴ ((2 4 - 1 2) × (2 4 ÷ 1 2)))
```

which gives:
```pometo_results
2 4
2 4
```

We can of course create `Agh forks` as well.

## Monadic Agh Fork Train Of Trains

Lets add another operator:

```pometo
A ← 1 2
(A-×⍴÷) 2 4
```

This is the equivalent of:

```apl
A ← 1 2
((1 2) - ((x 2 4) ⍴ (÷ 2 4)))
```

which gives:
```pometo_results
0.5 1.5
```

## Dyadic Agh Fork Train Of Trains

Easy expansion for this case:

```pometo
A ← 0.5 0.5
0.5 0.5 (A-×⍴÷) 2 4
```

This is the equivalent of:

```apl
A ← 2
(2 - ((0.5 0.5 x 2 4) ⍴ (0.5 0.5 ÷ 2 4)))
```

which gives:
```pometo_results
1.75 1.875
```

## Gotcha's

Assigning functions to variables implicitly casts them to trains. So:

```pometo
-+÷8
```

evaluates left associatively:

```pometo_results
¯0.125
```

But setting the functions in a variable gives a `monadic fgh fork`:

```pometo
A ← -+÷
A 8
```

results in:

```pometo_results
¯7.875
```