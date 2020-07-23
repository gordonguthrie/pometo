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


## Trains, Right Associate Funcs, Runtime Evaluation

Trains bring us problems.

We can chain things into random vectors:

```pometo
A ← 1 2
B ← 3 4
A B A 4 5
```

Which is an array:

```pometo_results
1 2 3 4 1 2 4 5 - blah-blah with boxes
```

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
10
```

But

```pometo
F ← -+÷
-+÷F 10
```

Resulting in:

```pometo_results
0.101010101
```

