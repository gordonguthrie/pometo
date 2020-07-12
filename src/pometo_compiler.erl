-module(pometo_compiler).

-export([
			compile/3
		]).

-include("parser_records.hrl").
-include("errors.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

% print_src is super useful but also not normally used, so...
-compile([{nowarn_unused_function, [{print_src, 1}]}]).

-record(sourcemap, {
					 pometo_line_no = none,
					 pometo_char_no = none,
					 description
					 }).

-define(LINE_NO_START,     1).
-define(EMPTY_RESULTS,     []).
-define(EMPTY_ACCUMULATOR, []).

% copy from https://github.com/basho/riak_ql/blob/develop/src/riak_ql_ddl_compiler.erl

compile(Functions, ModuleName, Str) when is_list(Functions) andalso
																				 is_list(ModuleName) ->
	SourceMap = #{},
	{Exports, _FunBodies} = lists:unzip(Functions),

	SourceMap1 = SourceMap#{?LINE_NO_START => #sourcemap{description = "module attribute"}},
	{ModAttr, LineNo1}  = make_modname(ModuleName, ?LINE_NO_START),

	SourceMap2 = SourceMap1#{LineNo1 => #sourcemap{description = "export attribute"}},
	{ExpAttr, LineNo2}  = make_exports(Exports, LineNo1),

	Records = [
	reset(?Q("-record('$ast¯', {"                        ++
															"do, "                   ++
															"args    = [], "         ++
															"line_no = none, "       ++
															"char_no = none"         ++
															"})."), LineNo1),
	reset(?Q("-record('$shape¯', {"                      ++
																"indexed    = false, " ++
																"dimensions = [],"     ++
																"forcing    = none,"   ++
																"type       = none,"   ++
																"line_no    = none,"   ++
																"char_no    = none"    ++
																"})."), LineNo1 + 1),
	reset(?Q("-record('$func¯', {"                            ++
															 "do             = false, "   ++
															 "type           = [],"       ++
															 "result         = explicit," ++
															 "shape_changing = false ,"   ++
															 "rank           = last ,"    ++
															 "line_no        = none,"     ++
															 "char_no        = none"      ++
															 "})."), LineNo1 + 2)
		],

	SourceMap3 = SourceMap2#{LineNo2 => #sourcemap{description = "parser records import definition"}},
	LineNo3 = LineNo2 + 3,

	{PublicFns,  LineNo4, SourceMap4} = make_public_fns(Exports,    LineNo3, ModuleName, SourceMap3, ?EMPTY_RESULTS),
	{PrivateFns, LineNo5, SourceMap5} = make_private_fns(Functions, LineNo4, ModuleName, SourceMap4, ?EMPTY_RESULTS),

	%% ?debugFmt("final SourceMap is ~p~n", [SourceMap5]),

	%% TODO turn SourceMap 5 into a lookup function for errors to map back to Pometo code

	AST = ModAttr    ++
				ExpAttr    ++
				Records    ++
				PublicFns  ++
				PrivateFns ++
				[{eof, LineNo5}],

	% io:format("AST is ~p~n", [AST]),

	case erl_lint:module(AST) of
		{ok, []} ->
			% uncomment for debugging
			% print_src(AST),
			load_BEAM(AST);
		{ok, [{"nofile", Errs}]} ->
			{error, format_lint_errors(Errs, SourceMap5, ?EMPTY_RESULTS, Str)}
		end.

%% This function is partially implemented because it will need to
%% get tested with run time errors to be useful and we can't yet trigger
%% run time errors
reset({attribute, _LineNo, record, {Record, Fields}}, NewLineNo) ->
	{attribute, NewLineNo, record, {Record, [reset(X, NewLineNo) || X <- Fields]}};
reset({attribute, _LineNo, include, Inc}, NewLineNo) ->
	{attribute, NewLineNo, include, Inc};
reset({record_field, _LineNo, Key}, NewLineNo) ->
	{record_field, NewLineNo, Key};
reset({record_field, _LineNo, Key, Value}, NewLineNo) ->
	{record_field, NewLineNo, Key, Value}.

format_lint_errors([], _SM, Errs, _Str) -> lists:reverse(Errs);
format_lint_errors([{_LineNo, erl_lint, {unused_var, V}} | T], SM, Errs, Str) ->
	Err = #error{type    = "UNUSED VARIABLE",
							 msg1    = "variable is unused",
							 msg2    = atom_to_list(V),
							 at_line = 1,
							 at_char = 1
							},
	format_lint_errors(T, SM, [Err | Errs], Str).

print_src(AST) ->
	Syntax = erl_syntax:form_list(AST),
		?debugFmt("source code is:~n~ts~n", [erl_prettypr:format(Syntax)]).

load_BEAM(AST) ->
	{ok, Mod, Bin} = compile:forms(AST),
	BeamFileName = "/tmp/" ++ atom_to_list(Mod) ++ ".beam",
		{module, Mod} = code:load_binary(Mod, BeamFileName, Bin).

make_private_fns([], LineNo, _ModuleName, SourceMap, Results) ->
	{lists:reverse(Results), LineNo, SourceMap};
make_private_fns([{{Fn, Arity, Args}, Body} | T], LineNo, ModuleName, SourceMap, Results) ->
	Desc = "private function of " ++
			 atom_to_list(Fn)       ++
			 "/"                    ++
			 integer_to_list(Arity),
	NewSourceMap = SourceMap#{LineNo => #sourcemap{description = Desc}},
	% we are about to add an extra line so bump the line no
	{NewBody, NewLineNo, NewSourceMap2} = make_body(Body, LineNo, NewSourceMap, ?EMPTY_RESULTS),
	Src = make_do_fn(ModuleName, Fn, Args) ++
			" -> " ++
			NewBody,
	make_private_fns(T, NewLineNo, ModuleName, NewSourceMap2, [?Q(Src) | Results]).

make_body([], LineNo, SourceMap, Results) ->
	Body = string:join(lists:reverse(Results), ",\n") ++ ".",
	{Body, LineNo, SourceMap};
make_body([H | T], LineNo, SourceMap, Results) ->
	NewSourceMap = make_source_map(H, LineNo, SourceMap),
	Line = make_line(H),
	make_body(T, LineNo + 1, NewSourceMap, [Line | Results]).

make_public_fns([], LineNo, _ModuleName, SourceMap, Results) ->
	{lists:reverse(Results), LineNo, SourceMap};
make_public_fns([{Fn, Arity, Args} | T], LineNo, ModuleName, SourceMap, Results) ->
	Desc = "public declaration of " ++
				 atom_to_list(Fn)         ++
				 "/"                      ++
				 integer_to_list(Arity),
	NewSourceMap = SourceMap#{LineNo => #sourcemap{description = Desc}},
	% we are about to add an extra line so bump the line no
	{NewLineNo, Body} = make_export_body(ModuleName, Fn, Args, LineNo),
	Src = atom_to_list(Fn) ++
				make_args(Args)  ++
				" ->"            ++
				Body,
	make_public_fns(T, NewLineNo + 1, ModuleName, NewSourceMap, [?Q(Src) | Results]).

make_args(Args) -> "(" ++ string:join(Args, ", ") ++ ")".

make_export_body(ModuleName, Fn, Args, LineNo) ->
	Body = "try "                                                ++
				 make_do_fn(ModuleName, Fn, Args)                      ++
				 "\n"                                                  ++
				 "catch\n"                                             ++
				 "    throw:E -> io:format(\"throwing ~p~n\", [E]),\n" ++
				 "               E\n"                                  ++
				 "end.\n",
	{LineNo + 4, Body}.

make_do_fn(ModuleName, Fn, Args) ->
	Hash = binary:bin_to_list(base16:encode(crypto:hash(sha, [ModuleName, atom_to_list(Fn), Args]))),
	Function = "do_"            ++
						 atom_to_list(Fn) ++
						 "_"              ++
						 Hash             ++
						 make_args(Args),
	lists:flatten(Function).

make_exports(Exports, LineNo) ->
	ReducedExports = [{Fn, Arity} || {Fn, Arity, _Args} <- Exports],
	{[{attribute, LineNo, export, ReducedExports}], LineNo + 1}.

make_modname(ModuleName, LineNo) ->
	{[{attribute, LineNo, module, list_to_atom(ModuleName)}], LineNo + 1}.

make_line(#'$ast¯'{do      = [{apply_fn, {Mod, Fun}}],
									 args    = Args,
									 line_no = LNo,
									 char_no = CNo})  ->
	make_apply_function_call([{Mod, Fun}], Args, apply_fn, LNo, CNo);
make_line(#'$ast¯'{do   = #'$shape¯'{},
									 args = #'$var¯'{name = Var}}) ->
	Var;
make_line(#'$ast¯'{do   = #'$shape¯'{dimensions = 0} = Shp,
									 args = Arg} = AST) ->
	NewShp = make_record(Shp),
	make_record(AST#'$ast¯'{do   = NewShp,
													args = Arg});
make_line(#'$ast¯'{do   = #'$shape¯'{} = Shp,
									 args = Args} = AST) ->
	NewShp = make_record(Shp),
	NewArgs = apply_to_args(fun maybe_make_record/1, Args),
	make_record(AST#'$ast¯'{do   = NewShp,
													args = NewArgs});
% when the variable is being set to a scalar or vector
make_line(#'$ast¯'{do   = 'let',
									 args = [Var, #'$ast¯'{do   = #'$shape¯'{},
																				 args = Args} = A | []]}) ->
	% strip the variable name and rename the do
	Src = case Args of
		#'$var¯'{name = V} -> atom_to_list(Var) ++
													" = "             ++
													V;
		_                  -> atom_to_list(Var) ++
													" = "             ++
													make_record(A)
	end,
	Src;
% when the variable is being set to the result of an expression
make_line(#'$ast¯'{do   = 'let',
									 args = [Var, Args]}) ->
	% strip the variable name and rename the do
	Src = case Args of
		#'$var¯'{name = V} -> atom_to_list(Var) ++
													" = "             ++
													V;
		_                  -> atom_to_list(Var) ++
													" = "             ++
													make_line(Args)
	end,
	Src;
make_line(#'$ast¯'{do   = do_fn,
									 args = Args}) ->
	"[" ++ string:join(Args, ", ") ++ "]";
make_line(#'$ast¯'{do      = #'$func¯'{}  = Func,
									 args    = Args}) ->
	make_function_call(Func, Args);
make_line(#'$func¯'{} = Func) ->
	make_record(Func);
make_line(X) when is_atom(X) ->
	atom_to_list(X);
make_line(X) ->
	X.

make_apply_function_call(Do, Args, Type, LNo, CNo) ->
	Func = make_fn_ast(Do, LNo, CNo),
	make_function_call2(Func, Args, Type).

make_function_call(#'$func¯'{type = Type} = Func, Args) ->
	make_function_call2(Func, Args, Type).

make_function_call2(Func, Args, Type) ->
	ExpFun = fun(A, Acc) ->
			L = make_line(A),
			[L | Acc]
	end,
	ExpArgs = lists:foldl(ExpFun, ?EMPTY_RESULTS, [Func | Args]),
	Src = "pometo_runtime:"                         ++
				atom_to_list(Type)                        ++
				"(["                                      ++
				string:join(lists:reverse(ExpArgs), ", ") ++
				"])",
	Src.

make_fn_ast(Args, LNo, CNo) -> NewArgs = [quote(X) || X <- Args],
					 #'$ast¯'{do      = do_fn,
					          args    = NewArgs,
										line_no = LNo,
										char_no = CNo}.

apply_to_args(Fn, Args) when is_list(Args) -> [Fn(X) || X <- Args];
apply_to_args(Fn, Args) when is_map(Args)  -> I = maps:iterator(Args),
																							apply_to_map_Val(Fn, Args, I).

apply_to_map_Val(_Fn, Map, none) -> Map;
apply_to_map_Val(Fn,  Map, I)    -> {K, V, NewI} = maps:next(I),
																		NewMap = maps:put(K, Fn(V), Map),
																		apply_to_map_Val(Fn, NewMap, NewI).


map_over_Key_Val(_Fn, _Map, Acc, none) -> Acc;
map_over_Key_Val(Fn,  Map,  Acc, I) 	 -> {K, V, NewI} = maps:next(I),
																					NewAcc = Fn(K, V),
																					map_over_Key_Val(Fn, Map, [NewAcc | Acc], NewI).


maybe_make_record(T) when is_tuple(T) -> make_record(T);
maybe_make_record(A) when is_atom(A)  -> atom_to_list(A);
maybe_make_record(X)                  -> X.

make_record(#'$ast¯'{do      = complex,
										 args    = [R, I],
										 line_no = LineNo,
										 char_no = CharNo}) ->
	"#'$ast¯'{"               ++
	"do = complex"            ++
	", "                      ++
	"args = ["                ++
	expand_arg(R)             ++
	", "                      ++
	expand_arg(I)             ++
	"], "                     ++
	"line_no = "              ++
	make_line_char_no(LineNo) ++
	", "                      ++
	"char_no = "              ++
	make_line_char_no(CharNo) ++
	"}";
make_record(#'$ast¯'{do              = Do,
										 args            = Args,
										 line_no         = LineNo,
										 char_no         = CharNo}) ->
	SrcArgs = case is_tuple(Args) of
		true  ->
			"args = "         ++
			make_record(Args) ++
			", ";
		false ->
			case is_list(Args) of
				true ->
					"args = ["        ++
					expand_args(Args) ++
					"], ";
				false ->
					"args = "         ++
					expand_arg(Args) ++
					", "
			end
	end,
	"#'$ast¯'{"                ++
	"do = "                    ++
	maybe_make_record(Do)      ++
	", "                       ++
	SrcArgs                    ++
	"line_no = "               ++
	make_line_char_no(LineNo)  ++
	", "                       ++
	"char_no = "               ++
	make_line_char_no(CharNo)  ++
	"}";
make_record(#'$shape¯'{indexed    = Indexed,
											 dimensions = Dims,
											 forcing    = Forcing,
											 type       = Type,
											 line_no    = LineNo,
											 char_no    = CharNo}) ->
	"#'$shape¯'{"             ++
	"indexed = "               ++
	atom_to_list(Indexed)      ++
	", "                       ++
	"dimensions = "            ++
	make_dimensions(Dims)      ++
	", "                       ++
	"forcing = "               ++
	atom_to_list(Forcing)      ++
	", "                       ++
	"type = "                  ++
	atom_to_list(Type)         ++
	", "                       ++
	"line_no = "               ++
	make_line_char_no(LineNo)  ++
	", "                       ++
	"char_no = "               ++
	make_line_char_no(CharNo)  ++
	"}";
make_record(#'$func¯'{do             = D,
											type           = T,
											result         = Re,
											shape_changing = S,
											rank           = Rk,
											line_no        = LNo,
											char_no        = CNo}) ->
	Quoted = case is_list(D) of
			true  -> lists:flatten("[" ++ [quote(X) || X <- D] ++ "]");
			false -> quote(D)
	end,
  "#'$func¯'{"            ++
  "do = "                 ++
  Quoted                  ++
  ", "                    ++
  "type = "               ++
  atom_to_list(T)         ++
  ", "                    ++
  "result = "             ++
  atom_to_list(Re)        ++
  ", "                    ++
  "shape_changing = "     ++
  atom_to_list(S)         ++
  ", "                    ++
  "rank = "               ++
  make_rank(Rk)           ++
  ", "                    ++
	"line_no = "            ++
	make_line_char_no(LNo)  ++
	", "                    ++
	"char_no = "            ++
	make_line_char_no(CNo)  ++
  "}";
make_record({'$var¯', V, _LineNo, _CharNo}) ->
	V.

make_rank(L) when is_list(L)    -> "[" ++ string:join([make_rank(X) || X <- L], ", ") ++ "]";
make_rank(N) when is_integer(N) -> integer_to_list(N);
make_rank(F) when is_float(F)   -> float_to_list(F);
make_rank(A) when is_atom(A)    -> atom_to_list(A).

expand_args(Args) -> string:join(apply_to_args(fun expand_arg/1, Args), ", ").

expand_arg(N) when is_integer(N) -> integer_to_list(N);
expand_arg(F) when is_float(F)   -> float_to_list(F);
expand_arg(A) when is_atom(A)    -> atom_to_list(A);
expand_arg(T) when is_tuple(T)   -> make_record(T);
expand_arg(M) when is_map(M)     -> make_indexed(M);
expand_arg(L) when is_list(L)    -> L.

make_indexed(Args) ->
	WriteMapElementFn = fun(K, V) ->
		S1 = integer_to_list(K),
		S2 = expand_arg(V),
		S1 ++ " => " ++ S2
	end,
	I = maps:iterator(Args),
	NewArgs = map_over_Key_Val(WriteMapElementFn, Args, ?EMPTY_ACCUMULATOR, I),
	"#{" ++ string:join(NewArgs, ", ") ++ "}".

make_dimensions(0)              -> "0";
make_dimensions(unsized_vector) -> "unsized_vector";
make_dimensions(Dimensions)     -> NewDs = [integer_to_list(D) || D <- Dimensions],
																	 "[" ++ string:join(NewDs, ", ") ++ "]".

make_line_char_no(none)                 -> "none";
make_line_char_no(N) when is_integer(N) -> integer_to_list(N).

quote({X, Y})                    -> "{" ++ quote(X) ++ ", " ++ quote(Y) ++   "}";
quote(N)      when is_integer(N) -> integer_to_list(N);
quote(F)      when is_float(F)   -> float_to_list(F);
quote(L)      when is_list(L)    -> "\"" ++ L ++ "\"";
quote(A)      when is_atom(A)    -> atom_to_list(A).

make_source_map(#'$ast¯'{do      = Do,
												 line_no = LNo,
												 char_no = CharNo}, LineNo, SourceMap) ->
	Desc = make_desc(Do),
	SM = #sourcemap{pometo_line_no = LNo,
								  pometo_char_no = CharNo,
									description    = Desc},
	SourceMap#{LineNo => SM}.

make_desc(#'$shape¯'{indexed    = Indexed,
										 dimensions = Dims,
										 type       = Type}) ->
	io_lib:format("~p Array (indexed:~p) with shape ~p~n", [Type, Indexed, Dims]);
make_desc(#'$func¯'{do             = D,
										type           = T,
										result         = Re,
										shape_changing = S,
										rank           = Rk}) ->
	io_lib:format("~p Func type:~p result: ~p shape changing:~p rank:~p~n", [D, T, Re, S, Rk]);
make_desc({Do, Attr}) -> atom_to_list(Do) ++ "_" ++ Attr;
make_desc(Do)         -> atom_to_list(Do).

