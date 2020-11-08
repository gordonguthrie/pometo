# Pometo Error Reference

This is the error reference document for Pometo - it ***should*** have a reference and probable cause of all errors encountered in Pometo - at compile and runtime.

## CAVEAT LECTOR/Reader Beware

As the code base is very primitive errors will be subject to enormous change.

The purpose of writing these error tests early is to guide the process of putting in place error handling and reporting through the lexer, parser and then both the runtime interpreter and the compiler - and into the REPL `Rappel`.

To that end, don't go mad writing error tests at this stage if your are thinking of contributing.

However the strategic aim is to have a single point of look up of error codes and where they can be generated and how they can be fixed.

Generally try and conform to the style of APL-like errors:
http://microapl.com/apl_help/ch_020_030_030.htm

# Index

* [`DOMAIN ERROR`](#domain-error)
* [`INDEX ERROR`](#index-error)
* [`LENGTH ERROR`](#length-error)
* [`SYNTAX ERROR`](#syntax-error)
* [`VARIABLE NOT DEFINED`](#variable-not-defined)
* [`VARIABLE REASSIGNED`](#variable-reassigned)

# DOMAIN ERROR

A `DOMAIN ERROR` is thrown when an attempt is made to invoke a function with an invalid type of argument. For instance the `reduce` function `/` requires either a `scalar` or a `vector` on the LHS. Invoking it with an `array` will trigger a `DOMAIN ERROR`:

```pometo
A ← 2 3 4 ⍴ ⍳ 24
B ← 2 2 ⍴ ⍳ 4
B /[2] A
```

```pometo_results
Error
A ← 2 3 4 ⍴ ⍳ 24
B ← 2 2 ⍴ ⍳ 4
B /[2] A
--^
DOMAIN ERROR [LHS must be a vector or a scalar: It has a shape of [2,2] ] on line 3 at character 3
```

# INDEX ERROR

## Invalid Axis

Trying to specify an Axis that is out of bounds will trigger an `INDEX ERROR`.

```pometo
A ← 2 3 4 ⍴ 1 2 3 4 5 6
2 /[6] A
```

Gives
```pometo_results
Error
A ← 2 3 4 ⍴ 1 2 3 4 5 6
2 /[6] A
--^
INDEX ERROR [Invalid Axis: 6 ] on line 2 at character 3
```

# LENGTH ERROR

## Mismatched Shape Errors

The error `LENGTH ERROR` is a runtime error that is thrown when an operation is attempted on an array that has the wrong shape. The message you get will depend on if the vector being processes is `eager` or `lazy`.

```pometo
1 2 3 + 4 5
```

An `eager` vector will be tested before invoking the dyadic function and give this error message:

```pometo_results
Error
1 2 3 + 4 5
^
LENGTH ERROR [dimensions mismatch in dyadic [\"+\"]: LHS dimensions \"3\" - RHS dimensions \"2\" ] on line 1 at character 1
```

A `lazy` vector doesn't know it its own length and is typically passed in from `Erlang` or `Elixir`. If this expression were evaluated where both the LHS and the RHS were lazy the error message would emerge from the bowels of the dyadic function slightly differently:

```pometo_lazy
Error
1 2 3 + 4 5
^
LENGTH ERROR [dimensions mismatch in dyadic [\"+\"]: ran out of matches after 2 elements ] on line 1 at character 1
```

## Axis Operations

Axis function expect an axis that matches to the shape being operated on and will throw a RANK ERROR is that is not the case:

```pometo
C ← 2 3 4 ⍴ ⍳24
⍴ ,[3 4] C
```

Will give:

```pometo_results
Error
C ← 2 3 4 ⍴ ⍳24
⍴ ,[3 4] C
--^
RANK ERROR [Invalid Axis: [3,4] ] on line 2 at character 3
```

This is true for float axis specifications also:

```pometo
C ← 2 3 4 ⍴ ⍳24
⍴ ,[4.5] C
```

Will give:

```pometo_results
Error
C ← 2 3 4 ⍴ ⍳24
⍴ ,[4.5] C
--^
RANK ERROR [Invalid Axis: 4.5 ] on line 2 at character 3
```
# RANK ERROR

## Rank Errors

If you try and invoke an operator with operands of the wrong rank you will get an error. For example the dyadic reduce operator `\` requires the left hand operand to be of one rank lower than the right. If the RHS is a scalar then this operation is invalid dyadically.

```pometo
1 +/ 1
```

Resulting in:

```pometo_results
Error
1 +/ 1
-----^
RANK ERROR [RHS must be an array: It is a scalar ] on line 1 at character 6
```

# SYNTAX ERROR

## Variable Name Errors

If you try and use an invalid character in a Variable declaration you will get an error. Invalid characters are whitespace, any of the APL/Pometo unicode symbols and the normal punctuation you find on your keyboard with the exception of `_` and `-`.

In concordance with normal `Erlang` practice variables in `Pometo` start with a capital Latin letter ie `A-Z`.

```pometo
MyVar≠99 ← 1 2 3
```

```pometo_results
Error
MyVar≠99 ← 1 2 3
---------^
SYNTAX ERROR [syntax error before: ← ] on line 1 at character 10
```

```pometo
MyVar~99 ← 1 2 3
```

```pometo_results
Error
MyVar~99 ← 1 2 3
---------^
SYNTAX ERROR [syntax error before: ← ] on line 1 at character 10
```

```pometo
myvar ← 1 2 3
```

```pometo_results
Error
myvar ← 1 2 3
^
SYNTAX ERROR [syntax error before: m ] on line 1 at character 1
```

# VARIABLE NOT DEFINED

## Undefined Variable Errors

You can't invoke a variable before it is defined.

```pometo
A ← 4 5 6 ⋄ B ← 6 7 ¯8 ⍝ including comments
D + C
```

will give the following error:

```pometo_results
Error
D + C
----^
VARIABLE NOT DEFINED [C: variable is not defined ] on line 2 at character 5


Error
D + C
^
VARIABLE NOT DEFINED [D: variable is not defined ] on line 2 at character 1
```

# VARIABLE REASSIGNED

## Variable Reassigment Errors

Like in `Erlang` variables in `Pometo` are immutable once you have run `A ← 1 2 3` then `A` has the value `1 2 3` as long as it remains in scope.

Redefining a variable will give you a `VARIABLE REASSIGNED` error.

```pometo
A ← 4 5 6 ⋄ A ← 6 7 ¯8
```

will give the following error:

```pometo_results
Error
A ← 4 5 6 ⋄ A ← 6 7 ¯8
------------^
VARIABLE REASSIGNED [A: was previously assigned on line 1 at char 1 ] on line 1 at character 13
```
