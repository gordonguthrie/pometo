# Smoke Tests

Because the documentation is also the test suite it is sometimes necessary to just bang out primitive smoke tests that would clutter the user-facing documentation.

This section is one such. It is a test suite for basic execution paths in `pometo_runtime`.

Any test failure arising from this documentation will cause a slew of errors across the test suite.

You can stop reading now ;-)

## Unitary Negation Smoke Tests

Simple negative integers:

```pometo
- 1
```

result it:

```pometo_results
¯1
```

Complex too:

```pometo
- 1J3
```

as you would expect:

```pometo_results
¯1J¯3
```

## Monadic Branches - Real Numbers Smoke Tests

### Scalar first

Not much to see here:

```pometo
+ 1
```

but yes it works

```pometo_results
1
```

### Now Vectors

Changing gear:

```pometo
+ 1 2
```

...the suspense is killing me...

```pometo_results
1 2
```

Different symbol for negation/subtractiona and being negative:

```pometo
- 1 2
```

Yup that funny raised negative....

```pometo_results
¯1 ¯2
```

Times is proper times symbol:

```pometo
× 1 ¯2
```

giving

```pometo_results
1 ¯1
```

Ditto divide:

```pometo
÷ 1 2 ¯4
```

...casts to floats, obvs:

```pometo_results
1.0 0.5 ¯0.25
```

## Monadic Branches - Complex Numbers Smoke Tests

This covers mixed vectors and scalars

Complex addition:

```pometo
+ 1j2 2J¯1
```

gives:

```pometo_results
1J¯2 2J1
```

And subtraction:

```pometo
- 1j2 2J¯1
```

gives:

```pometo_results
¯1J¯2 ¯2J1
```

Times tooL

```pometo
× 3J4
```

giving:

```pometo_results
0.6J0.8
```

And old fashioned division:

```pometo
÷ 3J4
```

```pometo_results
0.12J¯0.16
```

## Dyadic Branches - Real Numbers Smoke Tests

### Scalars first

scalars add up:

```pometo
1 + 2
```

to a `scalar`:

```pometo_results
3
```

### Now Mixed

`scalars` can be added to `arrays`:

```pometo
1 + 2 3
```

resulting in a new `array`:

```pometo_results
3 4
```

This works on both sides:

```pometo
2 3 + 1
```

...giving an `array` of course:

```pometo_results
3 4
```


### Now Pure Vectors

Pairwise `vector` addition

```pometo
4 5 + 1 2
```

gives:

```pometo_results
5 7
```

Ditto subtraction:

```pometo
2 3 - 1 2
```

gives:

```pometo_results
1 1
```

...and multiplication:

```pometo
4 5 × 1 2
```

as you would expect:

```pometo_results
4 10
```

and finally our old pal division:

```pometo
10 10 ÷ 1 2
```

with the cast to an `array` (a `vector` is just a 1 Dimensional `array`) of `float`s

```pometo_results
10.0 5.0
```

## Dyadic Branches - Complex Numbers Smoke Tests

Complex addition:

```pometo
4J2 5J¯1 + 3J3 2J5
```

as you would expect:

```pometo_results
7J5 7J4
```

and subtraction:

```pometo
4J2 5J1 - 3J3 2J5
```

giving:

```pometo_results
1J¯1 3J¯4
```

Dum-dee-dee multiplication:

```pometo
2J3 × 4j5
```

works too:

```pometo_results
¯7J22
```

And ending on division:

```pometo
51j2 ÷ 2j4
```

Its all good:

```pometo_results
5.5J¯10.0
```

## Dyadic Branches - Mixed Real And Complex Numbers Smoke Tests

Mixter maxter addition pairwise both ways:

```pometo
4J2 5 + 3 2J5
```

As expected:

```pometo_results
7J2 7J5
```

Similarly subtraction:

```pometo
4J2 5 - 3 2J5
```

giving:

```pometo_results
1J2 3J¯5
```

And complex multiplication:

```pometo
4J2 5 × 3 2J5
```

gives:

```pometo_results
12J6 10J25
```

And lastly division:

```pometo
4J2 10 ÷ 2 2J5
```

where floats go big for us:

```pometo_results
2.0J1.0 0.6896551724137931J¯1.7241379310344827
```

## Nested Arrays Smoke Tests

We can nest arrays:

```pometo
1 2 (3 4)
```

...and they print boxed:

```pometo_results
    ┌───┐
1 2 │3 4│
    └───┘
```

## Mixing Lazy And Eager Vectors Smoke Tests

The default tests set all vectors to either lazy or eager but dyadics need to be tested in mixed mode.

```pometo
A ← ⎕make_lazy 1 2 3
A + 3 4 5
```

should give

```pometo_results
4 6 8
```

and

```pometo
A ← ⎕make_lazy 1 2 3
3 4 5 + A
```

should give:

```pometo_results
4 6 8
```

## Mixing indexed And Unindexed Vectors Smoke Tests

The default tests set all vectors to either indexed or not indexed but dyadics need to be tested in mixed mode.

```pometo
A ← ⎕make_indexed 1 2 3
A + 3 4 5
```

should give:

```pometo_results
4 6 8
```

and

```pometo
A ← ⎕make_indexed 1 2 3
3 4 5 + A
```

should give

```pometo_results
4 6 8
```

## Oddly Shaped Results - Test That Formatter, Baby Smoke Tests

```pometo
A ← ⍳ 18
2 9 ⍴ A
```

should give:

```pometo_results
 1  2  3  4  5  6  7  8  9
10 11 12 13 14 15 16 17 18
```

## Parsing With Variables

```pometo
A ← 1 2 3
A 1
```

```pometo_results
1 2 3 1
```

## Variables As Arguments In Function Calls

```pometo
A ← 1 2 3
⎕print_trees A 2 3
```

```pometo_results
binglo
```

```pometo
A ← 1 2 3
B ← 4
⎕print_trees A B
```

```pometo_results
shape: [2]           
|                    
├-----------------┐  
|                 |  
shape: [3]        4  
|                    
├-----------┬--┐     
|           |  |     
1           2  3     
 on line 3 at character 14

```

plus a lazy AST of course:

```pometo_lazy
shape: [2]                      
|                               
├----------------------------┐  
|                            |  
shape: unsized_vector        4  
|                               
├----------------------┬--┐     
|                      |  |     
1                      2  3     
 on line 3 at character 14

```
