# Conway's Game Of Life

## Introduction

Conway's Game Of Life is famously an APL one-liner

```apl
life←{                                  ⍝ John Conway's "Game of Life".
    ↑1 ⍵∨.∧3 4=+/,¯1 0 1∘.⊖¯1 0 1∘.⌽⊂⍵  ⍝ Expression for next generation.
}
```

There is an even shorter version:

```
lifeJF ← {3=s-⍵∧4=s←{+/,⍵}⌺3 3⊢⍵}
```

Lets step through the operators:

* `life` is the name of the function and it has been set by the `let` operator `←` to be the dynfn defined between the pair of curly brackets `{` and `}`.

`apl` languages (and `pometo` is no exception) execute in the arabic fashion - right to left.

Let us look at the code this way.

* `⍵` represents the right hand variable in a function invocations so if a dfns was defined as `jim ← {...⍵}` and invoked as `jim 1 2 3` then `1 2 3` would be subsituted into the function body in place of `w`
* `⊂` is the `enclose` operator. If there is a vector `1 2 3` then `⊂ 1 2 3` created a vector of 1 element - the vector `1 2 3`. In Erlang terms this is something like going from `[1, 2, 3]` to `[[1, 2, 3]]`
* `⌽` - the reverse operator - axes are not what I think they are :-(
   * should probably chuck `⍉` and `⊖` in there as well
* `.` the dyadic inner product
    * overloaded in the syntax as `decimal point` and `name separator`
    * its a fold for vectors, but we don't want a fold only implementation
* `∘.` is the outer product
    * should implement `∘` compose as well
* `,` flattens out an array
    * easy to implement
* `+/` - `+/ 1 2 3` is the equivalent of `1 + 2 + 3` and we can replace `+` with `-`, `×` and any dyadic operator likewise
   * reductions are right folds - need three list traversals...
* `=` monadic equals
* `∨/` is the boolean `or` operator and `∧/` is the boolean `and`
* `⊂` in line with APL 2/IBM compatible version of mix
    * need to add extending which implies that we put types back into the `$¯¯⍴¯¯` record.

We will be on migration level 3

The shorter one requires:

* `⌹` the stencil

## Things I need to add

* `⍵` as a binding point
* `⊂` the enclose operator (and its syntactic sugar `(1 2 3)`

## things that would easy and handy

* '⍳' generates an index
* `⍴` that returns shape

