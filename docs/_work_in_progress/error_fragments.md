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

## Syntax Errrors

If you try and make a train with an odd no of elements - the left of which is a scalar you will get a syntax error

```pometo
A ← 1 2 3
(A+-÷) 4 5
```

resulting in:

```pometo_results
Error
A ← 1 2 3
(A+-÷) 4 5
--^
SYNTAX ERROR [Missing right argument: Cannot have a scalar or array as the LHS of a train with an odd number of functions (ie an atop) ] on line 2 at character 3
```