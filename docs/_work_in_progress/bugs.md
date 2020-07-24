## Bugs

***NOTE***: these bug reports should be integrated into the main documentation when they are fixed.

## Forwardslash Operator - evaluation order

All the tests are defective becuase they use commutative operators. If a non-commuting operator is used then the order of execution is shown to be arse-backwards:

Consider:

```pometo
÷/ 1 2 3
```

This is:
```pometo_results
1.5
```
and not as you might expect/as we currently return `0.166666667`.

The evaluation order should be `1÷(2÷3)` and not `(1÷2)÷3)`.

***NOTE***: all future operator documentation needs to be checked to make sure it uses non-commuting contructions.