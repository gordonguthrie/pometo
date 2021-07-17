# The stdlib function `⎕print_trees`

## Debuggin, debuggin, debuggin

`⎕print_trees` is a convenience function for developers writing new features for the `Pometo` runtime - not for users writing normal `Pometo` code.

`⎕print_trees` is a std function used to dump debugging output in `eunit` tests - it uses the `eunit` `?debugFmt` macro under the covers.

It prints to the shell using `io:format`, and to `eunit` using `?debugFmt` and also returns a comment (primarily for use in `Rappel`) which is printed out. The nature and format of the comment is undefined, or rather at the `Pometo` runtime developers convenience and is subject to change. It prints what it prints. Feel free to customise it for your development needs.

```pometo
⎕print_trees 1 2 (1 2) 3 4 (5 (6 7)) 8
```

```pometo_results
shape: [7]
|
---------------------------------------------
|           |  |           |  |  |           |
1           2  shape: [2]  3  4  shape: [2]  8
               |                 |
               ------------      ------------
               |           |     |           |
               1           2     5           shape: [2]
                                             |
                                             ------------
                                             |           |
                                             6           7
on line 1 at character 14
```

```pometo
A ← 1 2 3
B ← 4
C ← (4 5 6 )
⎕print_trees A B C
```

```pometo_results
blaghgag
 ```

```pometo
A ← 1
B ← A - + ÷
⎕print_trees B
```

```pometo_results
gfhfghfgh
```
