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
