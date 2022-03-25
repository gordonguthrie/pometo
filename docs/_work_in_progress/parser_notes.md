# Notes On The Implementation Of The Parser

A TRADFN is:

* a declaration
		* ∇
		* name
		* statement model
* a set of statements
* an ending
		* ∇

 A statement model is one of these options:

***Defined Functions***

| Result     |Niladic   |Monadic    |Dyadic     | Ambivalent  |
|------------|----------|-----------|-----------|-------------|
| None 	     | f 	      | f Y       |	X f Y 	  | {X} f Y     |
| Explicit 	 | R←f 	    | R←f Y     |	R←X f Y   | R←{X} f Y   |
| Suppressed | {R}←f    | {R}←f Y   | {R}←X f Y | {R}←{X} f Y |

An Ambivalent Function is one that might be Monadic or Dyadic depending on context (eg `+` is Ambivalent)

***Derived Functions*** produced by Monadic Operator:

| Result     | Monadic     | Dyadic       | Ambivalent     |
|------------|-------------|--------------|----------------|
| None       | (A op)Y     | X(A op)Y     | {X}(A op)Y     |
| Explicit   | R←(A op)Y   | R←X(A op)Y   | R←{X}(A op)Y   |
| Suppressed | {R}←(A op)Y | {R}←X(A op)Y | {R}←{X}(A op)Y |

***Derived Functions*** produced by Dyadic Operator:

| Result     | Monadic       | Dyadic         | Ambivalent       |
|------------|---------------|----------------|------------------|
| None       | (A op B)Y     | X(A op B)Y     | {X}(A op B)Y     |
| Explicit   | R←(A op B)Y   | R←X(A op B)Y   | R←{X}(A op B)Y   |
| Suppressed | {R}←(A op B)Y | {R}←X(A op B)Y | {R}←{X}(A op B)Y |


## Trains etc

```apl
 (f g h)⍵ ←→ (  f ⍵) g (  h ⍵)   ⍝ monadic fork (or monadic fgh fork)
 (A g h)⍵ ←→    A    g (  h ⍵)   ⍝ monadic fork (or monadic Agh fork)
 (  g h)⍵ ←→         g (  h ⍵)   ⍝ monadic atop

⍺(f g h)⍵ ←→ (⍺ f ⍵) g (⍺ h ⍵)   ⍝ dyadic fork (or dyadic fgh fork)
⍺(A g h)⍵ ←→    A    g (⍺ h ⍵)   ⍝ dyadic fork (or dyadic Agh fork)
⍺(  g h)⍵ ←→         g (⍺ h ⍵)   ⍝ dyadic atop
```

## Runtime Evaluation

Expressions of the form:

```pometo
1 +
```

Are always syntax errors

```pometo_results
Error
1 +
--^
SYNTAX ERROR [Invalid expression: No value at the RHS ] on line 1 at character 3
```

This is true if the value is replaced with a variable

```pometo
A ← 1
A +
```

```pometo_results
Error
A +
^
SYNTAX ERROR [Invalid expression: No value at the RHS ] on line 2 at character 1
```

This feeds into all evaluations.

![Parser Evaluation Decisions](../images/parser_decisions.png)

### Case 1

Sometimes we can't tell if a thing is a train or not:

```pometo
H ← ×
A ← + - H ÷ + -
A 10
```

gives

```pometo_results
99.0
```

This resolves to an execution tree of:

![Parser Decision Tree](../images/parser_decisions_1a.png)

Change `H` to a scalar:

```pometo
H ← 1
A ← + - H ÷ + -
A 10
```

gives

```pometo_results
Error
H ← 1
A ← + - H ÷ + -
A 10
----------^
SYNTAX ERROR [Missing right argument: Cannot have a scalar or array as the LHS of a train with an odd number of functions (ie an atop) ] on line 2 at character 11
```


### Case 2

If the variable switches to a function we get:

```pometo
A ← + - × ÷ + -
A 10
```

gives

```pometo_results
99.0
```

![Parser Decision Tree](../images/parser_decisions_2a.png)


The third case `+ - × ÷ + -` we know it is might be a train because it is assigned to a variable - so an array of type `func` or `maybe_func` is created and then passed at runtime to `pometo_runtime:maybe_run_monadic_train`

### Case 3

Now move the variable to the start of the

```
 __          ___           _  ___
 \ \        / / |         | ||__ \
  \ \  /\  / /| |__   __ _| |_  ) |
   \ \/  \/ / | '_ \ / _` | __|/ /
    \  /\  /  | | | | (_| | |_|_|
     \/  \/   |_| |_|\__,_|\__(_)
```

## Trains, Right Associate Funcs, Runtime Evaluation

Trains bring us problems.

We can chain things into random vectors:

```pometo
A ← 1
B ← 3
A B
```

Scalars resolve to vectors:

```pometo_results
1 3
```

This extends obviously:

```pometo
A ← 1
B ← 3
A B A B A
```

giving:

```pometo_results
1 3 1 3 1
```

Vectors resolve to a vector of vectors:

```pometo
A ← 1 2
B ← 3 4
A B A
```

Scalars resolve to vectors:

```pometo_results
┌───┐ ┌───┐ ┌───┐
│1 2│ │3 4│ │1 2│
└───┘ └───┘ └───┘
```

Things start getting messier when we mix in actual numbers into the expression. We don't know at write time if this will resolve as functions operating monadically on 4 5 or some data structure. We only know that ***after*** we have resolved the expressions that give values for `A` and `B`:

```pometo
A ← 1 2
B ← 3 4
A B A 4 5
```

Which is an array:

```pometo_results
┌───┐ ┌───┐ ┌───┐        
│1 2│ │3 4│ │1 2│   4   5
└───┘ └───┘ └───┘        
```

We can see this problem if we cut `4 5` down to `4`:

```pometo
A ← 1 2
B ← 3 4
A B A 4
```

Which produces:

```pometo_results
┌───┐ ┌───┐ ┌───┐    
│1 2│ │3 4│ │1 2│   4
└───┘ └───┘ └───┘    
```

The problem the parser has is that it doesn't know until `runtime` if it should box `4 5` into a vector or append them as two simple scalars:

But if A was a function:

```pometo
A ← -
B ← 3 4
A B A 4 5
```

This is the equivalent of:

```apl
- 3 4 - 4 5
```

It evaluates as:

```pometo_results
1 1
```

The execution sequence is:

| Function | Sign | Type    | LHS Argument  | RHS Argument    | Result          |
|----------|------|---------|---------------|-----------------|-----------------|
|minus     | `-`  | dyadic  | `array` [3 4] | `array` [ 4  5] | `array` [¯1 ¯1] |
|minus     | `+`  | monadic |               | `array` [¯1 ¯1] | `array` [ 1  1] |

For sufficiently complex stuff the bottom line cannot be evaluated until run time:

```pometo
A ← 1
B ← 2
C ← 3
A B C 4
```

```pometo_results
1 2 3 4
```

But...

```pometo
A ← +
B ← ÷
C ← -
A B C 4
```

```pometo_results
¯0.25
```

And...

```pometo
A ← 2
B ← ÷
C ← -
A B C 4
```

```pometo_results
¯0.5
```

This goes for trains of trains

```pometo
-+÷-+÷10
```

Giving:

```pometo_results
10.0
```

But

```pometo
F ← -+÷
-+÷F 10
```

Resulting in:

```pometo_results
0.10101010101010101
```

We need to be sure the parser can cope with fairly weird combos of vectors and trains and functions

```pometo
A ← - + ÷
B ← 2 9
C ← 4
D ← 5
6 7 + C D - B A 8 3
```

A is intepreted as a (⍺ f ⍵) g (⍺ h ⍵) - dyadic fork (or dyadic fgh fork)

| Fn    | Sign | Type    | LHS Argument   | RHS Argument       | Result             |
|-------|------|---------|----------------|--------------------|--------------------|
|divide | `÷`  | dyadic  | `array` [ 2 9] | `array` [ 8     3] | `array` [ 0.25  3] |
|minus  | `-`  | dyadic  | `array` [ 2 9] | `array` [ 8     3] | `array` [¯6     6] |
|plus   | `+`  | dyadic  | `array` [¯6 6] | `array` [ 0.25  3] | `array` [¯5.75  9] |
|minus  | `-`  | dyadic  | `array` [ 4 5] | `array` [¯5.75  9] | `array` [ 9.75 ¯4] |
|plus   | `+`  | dyadic  | `array` [ 6 7] | `array` [ 9.75 ¯4] | `array` [ 15.75 3] |

Giving

```pometo_results
15.75 3
```
