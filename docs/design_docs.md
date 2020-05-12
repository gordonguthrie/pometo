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

