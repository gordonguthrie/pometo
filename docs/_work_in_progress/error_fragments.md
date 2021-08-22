# Train Errors

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