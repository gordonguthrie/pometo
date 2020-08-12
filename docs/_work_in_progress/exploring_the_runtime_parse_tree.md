# Exploring The Runtime Parse Trees

## Using the `⎕debug` function

The best way to explore the runtime Parse Trees is with the `⎕debug` function in `stdlib`.

This

```pometo
A ← - + ÷
⎕debug A
```

Resolves to:

```pometo_results
In ⎕debug_fn
*******************************************************************************
This function array will be resolved at runtime
*******************************************************************************
as right associative this is:
([- + ÷])
as a monadic train this is:
((÷ ⍵) + (- ⍵))
as dyadic train this is:
((⍺ ÷ ⍵) + (⍺ - ⍵)) on line 1 at character 5
```

Becuase the 3 operators `-`, `+` and `÷` are not shape changing the function has been collapsed from the naive three pass `(- (+ (÷)))` to a single pass `([- + ÷])`.

If we stick a shape-changing operator in there the parse tree changes, obviously:

```pometo
A ← - +  ⍴ ÷
⎕debug A
```

We see that the 4 operator chain has been reduced to 3 - with `+` and `-` being combined into a single pass.

```pometo_results
In ⎕debug_fn
*******************************************************************************
This function array will be resolved at runtime
*******************************************************************************
as right associative this is:
([+ -] (⍴ (÷)))
as a monadic train this is:
(÷ ((⍴ ⍵) - (+ ⍵)))
as dyadic train this is:
(÷ ((⍺ ⍴ ⍵) - (⍺ + ⍵))) on line 1 at character 5
```

```pometo
A ← 1
B ← A - + ÷
⎕debug B
```

Gives

```pometo_results
something
```

