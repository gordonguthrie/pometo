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
RANK ERROR (RHS must be an array:It is a scalar) on line 1 at character 6
```