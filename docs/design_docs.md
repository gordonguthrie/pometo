# Design Docs for Pometo

## Introduction

These are design docs for Pometo

They are a work in progress - and are not definitive

## Purpose

To dump what I think I need to do down and get my arms around the work

## Scope

The scope of these documents are:

* developer flow
    * how you write APL in Pometo
* testing
    * writing end to end tests
    * embedding testing in documents
* the compiler toolchain
    * lexer
    * parser
    * output as `Liffey`
         * `Liffey` is a `LISP` `AST` defined as having a `to_string()` correspondence to `LFE` (`Lisp Flavoured Erlang`)
    * the REPL, `rappel`
* technical design decision
     * lazy evaluation of ⍴ ie accept lists as vectors)
          * requires a reserved `atom` - suggested `¯¯POMETO¯¯`
     * eager evaluation creates `map` data structures with integers as keys for fast lookup
     * the compiler deduces when to convert from lazy to eager and inserts a conversion step (if needed)
     * external calling functions to create data structures come in two flavours
         * `eager` and `lazy`

## Developer Flow

We need a shell, but how to represent it? We also need a virtual keyboard for onboarding.

A web-GUI makes sense then with an HTTP interface to a REPL - `rappel` is a cool name ;-)

## Overview

The basic kick off document is here:

https://medium.com/@gordonguthrie/the-beam-needs-an-apl-y-language-6c5c998ba6d

## Rowan's thoughts 2020_05_04

Heres an assortment of APL functions if you need testing targets.
I tried to pick ones ranging from simple (mean) to deviously complex (trav).
These were written for gnu apl, but nowadays dyalog is the most prominent implementation. There are subtle differences between the two I can help explain, or point you in the right direction for help.

I have written them in the DFN style (see https://en.wikipedia.org/wiki/Direct_function). There is also tradfns (https://aplwiki.com/wiki/Tradfn).

I am also asking people who have written FOSS implementations in the APL family about what weaknesses and possible changes they interpret in the language. I will compile these and fwd them.

```apl
mean ← {(⍴⍵)÷⍨+/⍵}
gmean ← {(÷⍴⍵)*⍨×/⍵} ⍝ geometric mean
hmean ← {÷(⍴⍵)÷⍨+/÷⍵} ⍝ harmonic mean
stdev ← { {0.5*⍨mean ⍵} {2*⍨⍵-⍨mean ⍵} ⍵ }
skew ← {+/(⍺*⍨⍵-mean ⍵)÷(⍴⍵)×⍺*⍨stdev ⍵} ⍝ ⍺ ← 3 → skew ◊ ⍺ ← 4 → kurtosis
phi ← {{1+÷⍵}⍣≡1⊣⍵}
eul ← {(÷2) *⍨ +/2*⍨⍵} ⍝ euclidian norm
comb ← { ⍵ {((⊂⍳⍺)×⍵)~¨0} ⍺ {⊂[1](⍺=+⌿⍵)/⍵} {(⍵⍴2)⊤⍳2*⍵} ⍵} ⍝ combinatorial. 3 comb 5
freq ← {(∪⍵){⍺,[÷2]+/⍺∘.=⍵}⍵} ⍝ frequency of numbers

sigmoid ← {÷1+*-⍵}
sigmoid_grad ← {⍵ × (1 - ⍵)}

⍝ box muller distribution. ⍺→depth ◊ ⍵→number of elements
⍝ Does not include setting the RNG seed!
gauss ← { {((¯2×⍟1⊃⍵)*0.5)×1○○2×2⊃⍵} ⍺ {⊂[1+⍳⍴,⍵](?(2,⍵)⍴⍺)÷⍺} ⍵}

⍝ higher order functions and rank operations:
hof ← {(⍶⍤1) ⍵}
diff ← { {∈{(1↑⍵)-⍨¯1↑⍵}¨2,/⍵} hof ⍵ } ⍝ convert a cumulative tally to an integer difference
⍝ diff ⍉10 10 ⍴ ⍳100 ⍝ difference down each column

⍝ traversal
⍝ see: https://dfns.dyalog.com/n_trav.htm
⍝      t         trav visits: t r a v
⍝    ┌─┼─┐       ravt visits: r a v t
⍝    r a v
trav ← {⊃(⍶ {⍺(⍶ trav ⍹)⍵} ⍹)⍨/⌽(⊂⍺ ⍶ ⍵),⍺ ⍹ ⍵} ⍝ dfs
gcd ← {⍺ | trav {⍺~0} ⍵} ⍝ gcd algorithm example
```


## Rowan's thoughts 2020_05_12

I was thinking of language details that have far-reaching consequences:
1. The Workspace
2. Scoping

Traditionally, APL has the concept of a "workspace" - a serializable program state that can be persisted to disk and resumed at a later point. It is useful in many data science workflows (as well as timesharing mainframes) - import some data, perform some analysis, write some functions, pick it up at a later point. More recent APL-family languages have left it out (specifically K - see note below), as it increases complexity (K is notoriously spartan in its aesthetic). It also has some impedance with modern VCS, as it historically was kept in an implementation specific binary format. I am unsure how well the concept of the workspace matches with erlang/BEAM. I am agnostic about its inclusion, but its something to keep in mind as you flesh out the language and the repl.

Note: I believe K uses a hierarchical global dictionary dubbed the K-Tree.
http://microapl.com/APL/apl_concepts_chapter2.html

Traditionally, APL globally scopes variables (egads!). In tradfn definitions, you would append the variable with a semicolon to locally scope. For example:

∇Z ← FOO A ;b ⍝ b is a local variable
    b←A+1
    Z←b
∇

I believe Dyalog locally scopes variables by default in DFN's (can confirm later). For example:
{b←⍵+1 ⋄ b} ⍝ b is locally scoped
I am also unsure if the scoping persists to nested lambdas within lambdas, ie:
{ b←⍵+1 ⋄ {b←⍵+2 ⋄ b} b} ⍝ does mutating the innermost b affect the outermost b?

I will have to start dyalog and run tests to check behavior.
Gnu APL on the other hand adopted a lambda style (and local variable definition) much closer in behavior to tradfns.
This is likely due to Jurgen's desire to match the Spec and keep implementation specific complexity at a minimum (he has spoken in great length about this in various emails to the mailing list).

⍝ gnu apl lambdas
{b⊣b←⍵+1;b} ⍝ b is local.
gnu APL uses ⊣ within lambdas as a quasi statement separator, and does not allow ⋄ or guards.

Personally, I prefer local-by-default, even if it will make the language "apl-ish" vs "mostly apl". However I am not the one writing the language, so I cede to your judgement.

There are other additional concepts I've had on my mind, like tacit functions, implicit function trains, and guards, but I don't think they are as important decisions as workspaces & scoping.

Very interested in how you envision the REPL. On one hand I prefer this:
1> ⍳3
      1 2 3

to this:
1> apl:stmt("⍳3");

but I also understand that accomplishing your goals with as little code/complexity as possible is important, and handling everything that the beam is capable of is not feasible. Is it possible to have a sub-repl? - start in erlang, switch to apl, write a function, return to erlang.

## Gordon's Working Notes 2020_05_13

`rappel` is now up as a repo with a proper README for getting started.
I have some very basic APL running, yay!

Basically:

```
1.1 2.2 + 3.3 4.4 
1.1 2.2 ¯ 3.3 4.4 
1.1 2.2 × 3.3 4.4 
1.1 2.2 ÷ 3.3 4.4 
```

The focus is next on getting the continuous integration and testing up.
What I want to do is smash three things into one:

* writing the specification
* writing the user manual
* writing the test suite

I am going to use markdown as the documentation of the language and parse that markdown for code blocks in `pometo` and have a test runner that executes them.

The idea is that you first write out what the feature should do in the documentation with the basic test cases (the user manual) and some edge case tests (the reference manual) and then these will fail in the test suite because you haven't implemented the features.
As we add the features, the tests should start passing and when they do, we commit to master.

This way reduces work dramatically ;-)

Rather than explain the toolchain I will demo it.

