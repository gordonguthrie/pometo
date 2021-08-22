# The stdlib function `⎕debug_fn`

## Debuggin, debuggin, debuggin

`⎕debug_fn` is a convenience function for developers writing new features for the `Pometo` runtime - not for users writing normal `Pometo` code.

`⎕debug_fn` is a std function used to dump debugging output in `eunit` tests - it uses the `eunit` `?debugFmt` macro under the covers.

It prints to the shell using `io:format`, and to `eunit` using `?debugFmt` and also returns a comment (primarily for use in `Rappel`) which is printed out. The nature and format of the comment is undefined, or rather at the `Pometo` runtime developers convenience and is subject to change. It prints what it prints. Feel free to customise it for your development needs.

You can explore how functions resolve with the `⎕debug_fn` function:

```pometo
A ← - + ÷
⎕debug_fn A
```

Giving

```pometo_results
In ⎕debug_fn
*******************************************************************************
This function array will be resolved at runtime
*******************************************************************************
As right associative this is:
([- + ÷])
As a monadic train this is:
((- ⍵) + (÷ ⍵))
As dyadic train this is:
((⍺ - ⍵) + (⍺ ÷ ⍵))
Where ⍺ is the LHS argument and ⍵ the RHS - on line 2 at character 11

```

As well as exploring function resolutions it also possible to printout data structures using the `⎕debug` function. 
