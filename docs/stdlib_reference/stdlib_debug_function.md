# The stdlib function `⎕debug`

## Debuggin, debuggin, debuggin

`⎕debug` is a convenience function for developers writing new features for the `Pometo` runtime - not for users writing normal `Pometo` code.

`⎕debug` is a std function used to dump debugging output in `eunit` tests - it uses the `eunit` `?debugFmt` macro under the covers.

It prints to the shell using `io:format`, and to `eunit` using `?debugFmt` and also returns a comment (primarily for use in `Rappel`) which is printed out. The nature and format of the comment is undefined, or rather at the `Pometo` runtime developers convenience and is subject to change. It prints what it prints. Feel free to customise it for your development needs.

```apl
⎕debug 1 2 (1 2) 3 4 (5 (6 7)) 8
```

```
In ⎕debug
*******************************************************************************
  from line 1 at character no 8
  Shape: type: mixed (indexed:false) with dimensions [7]
  arguments: 7
    1
    2
    element is an $ast¯:
      from line 1 at character no 13
      Shape: type: number (indexed:false) with dimensions [2]
      arguments: 2
        1
        2
    3
    4
    element is an $ast¯:
      from line 1 at character no 23
      Shape: type: mixed (indexed:false) with dimensions [2]
      arguments: 2
        5
        element is an $ast¯:
          from line 1 at character no 26
          Shape: type: number (indexed:false) with dimensions [2]
          arguments: 2
            6
            7
    8
*******************************************************************************
 on line 1 at character 8

```

As well as exploring data structures it also printout possible function resolutions.

```pometo
A ← - + ÷
⎕debug A
```

Giving

```pometo_results
In ⎕debug
*******************************************************************************
This function array will be resolved at runtime
*******************************************************************************
As right associative this is:
([- + ÷])
As a monadic train this is:
((- ⍵) + (÷ ⍵))
As dyadic train this is:
((⍺ - ⍵) + (⍺ ÷ ⍵))
Where ⍺ is the LHS argument and ⍵ the RHS - on line 2 at character 8

```

***Note For Developers***: this page is not marked up for tests. The debug functions show the internal state of the data and programme at a point in times which can have variations for indexed, etc, etc. There are manual and not generated tests for the function.