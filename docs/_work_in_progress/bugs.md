## Bugs

## How To Use This Page

The bugs page is a standing work-in-progress page.

Bugs should be appended - with a section per bug. (if this page is currently empty look at the history of this page in github to see examples of how it is used).

Be sure to mark the ***correct*** results with a `pometo_results` block - if you want to show what `pometo` returns mark that block `apl`.

When a bug is fixed it should be imported into the main documentation - and if appropriate a smoke test should be created.

This document will therefore fill up and empty in the natural progress of things.

***NOTE***: these bug reports should be integrated into the main documentation when they are fixed.

## Forwardslash Operator - Evaluation Order

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