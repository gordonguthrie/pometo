# Points For Improvement

This section outlines points where is might make sense to improve things. It is substantially naive implementations where a refactor could result in a speed up.

## Things Wut Is Bad, So Bad, So Very Bad...

The order of keys in a map is undefined but also if your keys are positive integers it is also positive integers.

At some stage in the future this might change and then the very gates of hell will be ripped open, but hey! you only live once.

## The Replicate `/` function over lazy lists

This does a full pass of the RHS args to get the length - that could be caught by a termination clause in iterate reducing one list scan