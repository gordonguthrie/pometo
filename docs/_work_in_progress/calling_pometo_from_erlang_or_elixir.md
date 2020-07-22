# Calling Pometo From Erlang Or Elixir

## Overview

The design brief for `Pometo` is that it is an auxiliary language for the `BEAM`.

To that end the normal `Erlang` or `Elixir` developer should simply find libraries on `hex` and use them in their code without knowing of caring that they are written in `pometo`.

## How it will work

***NOTE***: this is a sketch until we get `tradfuns` in properly.

The `pometo` developer will write a module that looks something like this:

```apl
mymodule

-type(....) ⍝ some type specification
∇ ret ← incr X
	ret ← 1 + X
∇
```

This will be saved as `mymodule.pomo`

This will then be compiled with `rebar3 compile` to either `pometo_mymodule.beam` or `Pometo.Mymodule.beam` depending on a compiler setting for the `Erlang` or `Elixir` namespace.

The actual `Erlang` Source will look something like this:

```erl
-module(pometo_mymodule).

% export for Erlang
-export([
					incr/1
				]).

% export for calling from another Pometo function
-export([
					rt_incr/1
				]).


-record('$ast¯',
	{do, args = [], line_no = none, char_no = none}).

-record('$shape¯',
	{indexed = false, dimensions = [], forcing = none,
	 type = none, line_no = none, char_no = none}).

-record('$func¯',
	{do = false, type = [], construction = primitive,
	 result = explicit, shape_changing = false, rank = last,
	 line_no = none, char_no = none}).

-record('$op¯',
	{op, fns = [], line_no = none, char_no = none}).


% call from `Erlang` or `Elixir`
incr(#'ast¯'{} = AST)   -> incr2(AST);
incr(L) when is_list(L) -> incr2(pometo_runtime:make_vector(L));
incr(X)                 -> incr2(pometo_runtime:make_scalar(X));

% when this function is called from with another pometo fn this is the entry point
rt_incr(AST) -> do_incr_11cdad4092f75b9358d539fc59d9f522ca8187eb(AST).

incr2(Term) ->

		try
			#'$ast¯{args = Args} = do_incr_11cdad4092f75b9358d539fc59d9f522ca8187eb(AST),
			pometo_runtime:maybe_unindex(Args)
		catch
			E -> format_error(E)
		end.

do_incr_11cdad4092f75b9358d539fc59d9f522ca8187eb(AST) ->
	pometo_runtime:monadic([#'$func¯'{do = ["+"],
						 type = dyadic, result = explicit,
						 shape_changing = false, rank = none,
						 line_no = 1, char_no = 3},
				 #'$ast¯'{do =
					#'$shape¯'{indexed = false,
							 dimensions = 0,
							 forcing = none,
							 type = number, line_no = 1,
							 char_no = 1},
						args = 1, line_no = 1, char_no = 1},
				 AST]).

% reformat the Erlang error into the Pometo source using the SourceMap
format_error(E) ->
	SourceMap = #{1 => {sourcemap,none,none,"module attribute"},
								2 => {sourcemap,none,none,"export attribute"},
								3 =>
										{sourcemap,none,none,
												"parser records import definition"},
								7 => {sourcemap,none,none,"public declaration of run/0"},
								12 =>
										{sourcemap,1,1,
												[[91,["\"+\""],93],
												32,70,117,110,99,32,116,121,112,101,58,
												"monadic",32,114,101,115,117,108,116,58,
												32,"explicit",32,115,104,97,112,101,32,
												99,104,97,110,103,105,110,103,58,"false",
												32,114,97,110,107,58,"none",10]}},
	pometo_runtime:reformat(E, SourceMap).
```

There is a function `print_src` in `pometo_compiler.erl` and normally the call to it is commented out, but you can uncomment it and see the `src` files. There will also be a compiler flag to output the `src` as well as the `beam` files in the fullness of time.

## Calling From Erlang

To use this function from with the `Erlang` shell you would write:

```erlang
1> A = pometo_mymodule:incr(1).
2
2> B = pometo_mymodule:incr([1 2 3]).
[2, 3, 4]
```

Once we have the type description sorted out we can make the function return different things - opaque `pometo` terms which can be stashed in a state record, etc, etc or unindexed plain `Erlang` data structures (ie lists).

In addition there will be helper functions like `pometo:new/2` which will take the form `pometo:new(scalar, 1)` or `pometo:new(vector, [1, 2, 3])` or `pometo:new(boolean, 5)` and so on and so forth.