# The stdlib function `⎕debug_fn`

## Debuggin, debuggin, debuggin

`⎕debug+fn` is a convenience function for developers writing new features for the `Pometo` runtime - not for users writing normal `Pometo` code.

`⎕debug_fn` is a std function used to dump debugging output in `eunit` tests - it uses the `eunit` `?debugFmt` macro under the covers.

It prints to the shell using `io:format`, and to `eunit` using `?debugFmt` and also returns a comment (primarily for use in `Rappel`) which is printed out. The nature and format of the comment is undefined, or rather at the `Pometo` runtime developers convenience and is subject to change. It prints what it prints. Feel free to customise it for your development needs.

```apl
A ← - + ÷
⎕debug A
```