# The stdlib function `⎕print_trees`

## Debuggin, debuggin, debuggin

`⎕print_trees` is a convenience function for developers writing new features for the `Pometo` runtime - not for users writing normal `Pometo` code.

`⎕print_trees` is a std function used to dump debugging output in `eunit` tests - it uses the `eunit` `?debugFmt` macro under the covers.

It prints to the shell using `io:format`, and to `eunit` using `?debugFmt` and also returns a comment (primarily for use in `Rappel`) which is printed out. The nature and format of the comment is undefined, or rather at the `Pometo` runtime developers convenience and is subject to change. It prints what it prints. Feel free to customise it for your development needs.

If you are helping develop the compiler or interpreter you can use the underlying primitive functions when debugging:

```
  Tree = pometo_stdlib:print_trees('$ast¯'{} = AST),
  PrintableOutput = pometo_runtime_format:format(Tree),
  io:format("PrintableOutput is ~ts~n", [PrintableOutput]),
```

## Beware

```code
  ____
 |  _ \
 | |_) | _____      ____ _ _ __ ___
 |  _ < / _ \ \ /\ / / _` | '__/ _ \
 | |_) |  __/\ V  V / (_| | | |  __/
 |____/ \___| \_/\_/ \__,_|_|  \___|
```

If you try and print out a structure containing a degenerate multidimensional scalar of type array it will blow up. See the `Stdlib Print Trees Function` documentation.

See the documentation on `iota (⍳) the index function` on how to create such degenerate forms.

There are a number of possible reasons for this:

1 its a bug, the print tree function is defective
2 degenerate forms should be printable but the current structure of the Abstract Syntax Tree is defective
3 the developer is an idiot
4 some combination of points 1, 2 and 3

## Printing trees in Rappel

Here are some examples:

```pometo
A ← 1
⎕print_trees A
```

gives

```pometo_results
1  
 on line 2 at character 14

```

```pometo
⎕print_trees 1 2 (1 2) 3 4 (5 (6 7)) 8
```

```pometo_results
shape: [7]/mixed                                                                            
|                                                                                           
├-----------------┬--┬---------------------┬--┬--┬---------------------------------------┐  
|                 |  |                     |  |  |                                       |  
1                 2  shape: [2]/number     3  4  shape: [2]/mixed                        8  
                     |                           |                                          
                     ├------------------┐        ├-----------------┐                        
                     |                  |        |                 |                        
                     1                  2        5                 shape: [2]/number        
                                                                   |                        
                                                                   ├------------------┐     
                                                                   |                  |     
                                                                   6                  7     
 on line 1 at character 14

```

Obviously lazy ASTs print differently:

```pometo_lazy
shape: unsized_vector/mixed                                                                            
|                                                                                                      
├----------------------------┬--┬---------------------┬--┬--┬---------------------------------------┐  
|                            |  |                     |  |  |                                       |  
1                            2  shape: [2]/number     3  4  shape: [2]/mixed                        8  
                                |                           |                                          
                                ├------------------┐        ├-----------------┐                        
                                |                  |        |                 |                        
                                1                  2        5                 shape: [2]/number        
                                                                              |                        
                                                                              ├------------------┐     
                                                                              |                  |     
                                                                              6                  7     
 on line 1 at character 14

```

```pometo
A ← 1 2 3
B ← 4
C ← (4 5 6 )
⎕print_trees A B C
```

```pometo_results
shape: [3]/runtime                                    
|                                                     
├-------------------------┬--┐                        
|                         |  |                        
shape: [3]/number         4  shape: [3]/number        
|                            |                        
├-------------------┬--┐     ├------------------┬--┐  
|                   |  |     |                  |  |  
1                   2  3     4                  5  6  
 on line 4 at character 14

```

This gives a different result in the interpreter:

```pometo_interpreted
shape: [3]/maybe_func                                    
|                                                        
├----------------------------┬--┐                        
|                            |  |                        
shape: [3]/number            4  shape: [3]/number        
|                               |                        
├----------------------┬--┐     ├------------------┬--┐  
|                      |  |     |                  |  |  
1                      2  3     4                  5  6  
 on line 4 at character 14

```

Plus a lazy AST too:

```pometo_lazy
shape: [3]/runtime                                              
|                                                               
├-----------------------------------┬--┐                        
|                                   |  |                        
shape: unsized_vector/number        4  shape: [3]/number        
|                                      |                        
├-----------------------------┬--┐     ├------------------┬--┐  
|                             |  |     |                  |  |  
1                             2  3     4                  5  6  
 on line 4 at character 14

```

```pometo
A ← 1
B ← A - + ÷
⎕print_trees B
```

```pometo_results
shape: [4]/maybe_func           
|                               
├----------------------┬--┬--┐  
|                      |  |  |  
1                      -  +  ÷  
 on line 3 at character 14

```

```pometo
A ← 1 2
B ← 3
⎕print_trees A B
```

```pometo_results
shape: [2]/runtime        
|                         
├----------------------┐  
|                      |  
shape: [2]/number      3  
|                         
├-------------------┐     
|                   |     
1                   2     
 on line 3 at character 14

```

This has a different view in the interpreter:

```pometo_interpreted
shape: [2]/maybe_func        
|                            
├-------------------------┐  
|                         |  
shape: [2]/number         3  
|                            
├----------------------┐     
|                      |     
1                      2     
 on line 3 at character 14

```

Obviously lazy ASTs print differently:

```pometo_lazy
shape: [2]/runtime                  
|                                   
├--------------------------------┐  
|                                |  
shape: unsized_vector/number     3  
|                                   
├-----------------------------┐     
|                             |     
1                             2     
 on line 3 at character 14

```