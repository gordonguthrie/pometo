# Notes On The Implementation Of The Parser

A TRADFN is:
 * a declaration
 		* ∇
 		* name
 		* statement model
 * a set of statements
 * an ending
 		* ∇

 A statement model is one of these options:

***Defined Functions***

| Result     |Niladic   |Monadic    |Dyadic     | Ambivalent  |
|------------|----------|-----------|-----------|-------------|
| None 	     | f 	      | f Y       |	X f Y 	  | {X} f Y     |
| Explicit 	 | R←f 	    | R←f Y     |	R←X f Y   | R←{X} f Y   |
| Suppressed | {R}←f    | {R}←f Y   | {R}←X f Y | {R}←{X} f Y |

An Ambivalent Function is one that might be Monadic or Dyadic depending on context (eg `+` is Ambivalent)

***Derived Functions*** produced by Monadic Operator:

| Result     | Monadic     | Dyadic       | Ambivalent     |
|------------|-------------|--------------|----------------|
| None       | (A op)Y     | X(A op)Y     | {X}(A op)Y     |
| Explicit   | R←(A op)Y   | R←X(A op)Y   | R←{X}(A op)Y   |
| Suppressed | {R}←(A op)Y | {R}←X(A op)Y | {R}←{X}(A op)Y |

***Derived Functions*** produced by Dyadic Operator:

| Result     | Monadic       | Dyadic         | Ambivalent       |
|------------|---------------|----------------|------------------|
| None       | (A op B)Y     | X(A op B)Y     | {X}(A op B)Y     |
| Explicit   | R←(A op B)Y   | R←X(A op B)Y   | R←{X}(A op B)Y   |
| Suppressed | {R}←(A op B)Y | {R}←X(A op B)Y | {R}←{X}(A op B)Y |

A STATEMENT is:

	* an EXPR
	* a LABEL
	* a SEPARATOR
	* A COMMENT

An EXPR is:
	* an ARRAY EXPR
	* a FUNCTION EXPR
	* (this include FUNCTION ASSIGNEMENT - ie LETs)

A LET is:

	* a Vector
	* a FN
	* the result of evaluating a FN

At the lowest level we need:

Funs -> Fun
Funs -> Fun Op
Funs -> Funs Fun