# Pometo Error Reference

This is the error reference document for Pometo - it ***should*** have a reference and probable cause of all errors encountered in Pometo - at compile and runtime.

## CAVEAT LECTOR/Reader Beware

As the code base is very primitive errors will be subject to enormous change.

The purpose of writing these error tests early is to guide the process of putting in place error handling and reporting through the lexer, parser and then both the runtime interpreter and the compiler - and into the repl `Rappel`.

To that end, don't go mad writing error tests at this stage if your are thinking of contributing.

However the strategic aim is to have a single point of look up of error codes and where they can be generated and how they can be fixed.

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
-----^
SYNTAX ERROR (invalid token:≠) on 1 at 6
```

```pometo
MyVar~99 ← 1 2 3
```

```pometo_results
Error
MyVar~99 ← 1 2 3
-----^
SYNTAX ERROR (invalid token:~) on 1 at 6
```

```pometo
myvar ← 1 2 3
```

```pometo_results
Error
myvar ← 1 2 3
^
SYNTAX ERROR (invalid token:m) on 1 at 1

Error
myvar ← 1 2 3
-^
SYNTAX ERROR (invalid token:y) on 1 at 2

Error
myvar ← 1 2 3
--^
SYNTAX ERROR (invalid token:v) on 1 at 3

Error
myvar ← 1 2 3
---^
SYNTAX ERROR (invalid token:a) on 1 at 4

Error
myvar ← 1 2 3
----^
SYNTAX ERROR (invalid token:r) on 1 at 5
```

# VARIABLE REASSIGNED

## Variable Reassigment Errors

Like in `Erlang` variables in Erlang are immutable once you have run `A ← 1 2 3` then `A` has the value `1 2 3` as long as it remains in scope.

Redefining a variable will give you a `VARIABLE REASSIGNED` error.

```pometo
"A ← 4 5 6 ⋄ A ← 6 7 ¯8
```

will give the following error:

```pometo_results
Error
A ← 4 5 6 ⋄ A ← 6 7 ¯8
------------^
VARIABLE REASSIGNED (A:was previously assigned on line 1 at char 1) on line 1 at character 13
```

# VARIABLE NOT DEFINED

## Undefined Variable Errors

You can't invoke a variable before it is defined.

```pometo
A ← 4 5 6 ⋄ B ← 6 7 ¯8 ⍝ including comments
A + C
```

will give the following error:

```pometo_results
ERROR FIX ME UP
```