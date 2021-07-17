# Exploring The Runtime Parse Trees

## Living With Amiguity

`apl` has an ambiguous grammer and `yecc` is a `LALR` parser - you might be forgiven for thinking that therefore `pometo` can never work. Well yes, and no. The output of the `pometo` parser is not unambiguous. One of the parser outputs is an `$ast¯` record that looks like this:

```erlang
#'$ast¯'{do = #'$shape¯'{type = func}}
#'$ast¯'{do = #'$shape¯'{type = maybe_func}}
```

The ambiguity is that expressions like `- + ÷ A ×` can only be resolved once we know if `A` resolves to a `function`, `operator` or `value`.

The arguments of shape of type `func` or `maybe_func` is simply a list of `funs` in the first case, or mixed `func`s and `var`s in the second.

At runtime we have enough information to resolve the ambiguity and we can push decisions to runtime with `$ast¯` `do`s of `defer_evaluation`.

If you type an expression directly into `rappel`:

```pometo
- + ÷
```

it will print out in the shell as:

```pometo_results
As right associative this is:
([- + ÷])
As a monadic train this is:
((- ⍵) + (÷ ⍵))
As dyadic train this is:
((⍺ - ⍵) + (⍺ ÷ ⍵))
Where ⍺ is the LHS argument and ⍵ the RHS - on line 1 at character 1

```

In this case the three functions `-`, `+` and `÷` have been collapsed into a single-pass function for right association.


## Using the `⎕debug` function


You can also explore the runtime Parse Trees is with the `⎕debug` function in `stdlib`.

This expression:

```pometo
A ← - + ÷
⎕debug A
```

resolves to:

```pometo_results
In ⎕debug
*******************************************************************************
This function array will be resolved at runtime
*******************************************************************************
As right associative this is:
([- + ÷])
As a monadic train this is:
((- ⍵) + (÷ ⍵))
As dyadic train this is:
((⍺ - ⍵) + (⍺ ÷ ⍵))
Where ⍺ is the LHS argument and ⍵ the RHS - on line 2 at character 8

```

Becuase the 3 operators `-`, `+` and `÷` are not shape changing the function has been collapsed from the naive three pass `(- (+ (÷)))` to a single pass `([- + ÷])`.

If we stick a shape-changing operator in there the parse tree changes, obviously:

```pometo
A ← - +  ⍴ ÷
⎕debug A
```

We see that the 4 operator chain has been reduced to 3 - with `+` and `-` being combined into a single pass.

```pometo_results
In ⎕debug
*******************************************************************************
This function array will be resolved at runtime
*******************************************************************************
As right associative this is:
([- +] (⍴ (÷)))
As a monadic train this is:
(- ((+ ⍵) ⍴ (÷ ⍵)))
As dyadic train this is:
(- ((⍺ + ⍵) ⍴ (⍺ ÷ ⍵)))
Where ⍺ is the LHS argument and ⍵ the RHS - on line 2 at character 8

```

By contrast the 3 operator chain:
```pometo
A ← 1
B ← A - + ÷
⎕debug B
```

results in a `SYNTAX ERROR`:

```pometo_results
In ⎕debug
*******************************************************************************
This function array will be resolved at runtime
*******************************************************************************
As right associative this is:
([- + ÷] 1)
As a monadic train this is:
\"SYNTAX ERROR [Invalid expression: No value at the RHS ]\"
As dyadic train this is:
\"SYNTAX ERROR [Invalid expression: No value at the RHS ]\"
Where ⍺ is the LHS argument and ⍵ the RHS - on line 3 at character 8

```

