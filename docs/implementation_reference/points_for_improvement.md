# Points For Improvement

This section outlines points where is might make sense to improve things. It is substantially naive implementations where a refactor could result in a speed up.

## Things Wut Is Bad, So Bad, So Very Bad...

The order of keys in a map is undefined but also if your keys are positive integers it is also positive integers.

At some stage in the future this might change and then the very gates of hell will be ripped open, but hey! you only live once.

## The Replicate `/` function over lazy lists

This does a full pass of the RHS args to get the length - that could be caught by a termination clause in iterate reducing one list scan

## Sort out the CSS

Not sorted for `apl` and `apl_results` in Jekyll.

## Ranks handling

Ranks are handled as a constant vector in the parser and not expressions that return a vector (yet)

## Dialyzer

mais, oui

## Unused variable definitions

I got an unresolved blowup that I think was a failure to handle undeclared variables in different ways

Needs some tests in smoke testing

## Zeros And Negative Ranks

Writing the reduce operator which has a special case of 0 operand exposed that needs to be handled separately exposed a weakness in our documentation. We need to collect all the use cases and review all documents to ensure that they cover them.

Ditto for negative ranks which sometimes work