-module(pometo_compiler).

-export([
			compile/2,
			compile/1
		]).

-include("parser_records.hrl").
-include("errors.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-record(sourcemap, {
					 pometo_line_no = none,
					 pometo_char_no = none,
					 description
				   }).

-define(LINE_NO_START, 1).
-define(EMPTY_RESULTS, []).

%% fake compile point until we get full compile syntax
%% gotta call the damn module something in the meantime
compile(#liffey{} = L) -> compile(L, pometo).

% copy from https://github.com/basho/riak_ql/blob/develop/src/riak_ql_ddl_compiler.erl

compile(Functions, ModuleName) when is_list(Functions) andalso
                                    is_list(ModuleName) ->
	SourceMap = #{},
	{Exports, _FunBodies} = lists:unzip(Functions),

	SourceMap1 = SourceMap#{?LINE_NO_START => #sourcemap{description = "module attribute"}},
	{ModAttr, LineNo1}  = make_modname(ModuleName, ?LINE_NO_START),

	SourceMap2 = SourceMap1#{LineNo1 => #sourcemap{description = "export attribute"}},
	{ExpAttr, LineNo2}  = make_exports(Exports, LineNo1),

	Records = [
		reset(?Q("-record(liffey, {"                  ++
							       "op, "             ++
				                   "args    = [], "   ++
				                   "line_no = none, " ++
				                   "char_no = none"   ++
				                   "})."), LineNo1),
		reset(?Q("-record('¯¯⍴¯¯', {"                      ++
							        "style      = eager, " ++
                                    "indexed    = false, " ++
                                    "dimensions = [],"     ++
                                    "line_no    = none,"   ++
                                    "char_no    = none"    ++
                                    "})."), LineNo1 + 1)
		],

	SourceMap3 = SourceMap2#{LineNo2 => #sourcemap{description = "parser records import definition"}},
	LineNo3 = LineNo2 + 2,

	{PublicFns,  LineNo4, SourceMap4} = make_public_fns(Exports,    LineNo3, ModuleName, SourceMap3, ?EMPTY_RESULTS),
	{PrivateFns, LineNo5, SourceMap5} = make_private_fns(Functions, LineNo4, ModuleName, SourceMap4, ?EMPTY_RESULTS),

	% ?debugFmt("final SourceMap is ~p~n", [SourceMap5]),

	%% TODO turn SourceMap 5 into a lookup function for errors to map back to Pometo code

	AST = ModAttr    ++
		  ExpAttr    ++
		  Records    ++
		  PublicFns  ++
		  PrivateFns ++
		  [{eof, LineNo5}],

	case erl_lint:module(AST) of
        {ok, []} ->
			% print_src(AST),
			load_BEAM(AST);
        {ok, [{"nofile", Errs}]} ->
			{error, format_lint_errors(Errs, SourceMap5, ?EMPTY_RESULTS)}
    end.

%% This function is partially implemented because it will need to
%% get tested with run time errors to be useful and we can't yet trigger
%% run time errors
reset({attribute, _LineNo, record, {Record, Fields}}, NewLineNo) ->
	{attribute, NewLineNo, record, {Record, [reset(X, NewLineNo) || X <- Fields]}};
reset({record_field, _LineNo, Key}, NewLineNo) ->
	{record_field, NewLineNo, Key};
reset({record_field, _LineNo, Key, Value}, NewLineNo) ->
	{record_field, NewLineNo, Key, Value}.

format_lint_errors([], _SM, Errs) -> lists:reverse(Errs);
format_lint_errors([{_LineNo, erl_lint, {unused_var, V}} | T], SM, Errs) ->
	Err = #error{type    = "UNUSED VARIABLE",
                 msg1    = "variable is unused",
                 msg2    = atom_to_list(V),
                 expr    = "dunno? FIXME",
                 at_line = 1,
                 at_char = 1
                },
	format_lint_errors(T, SM, [Err | Errs]).

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
		  " ->" ++
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
	Body = make_export_body(ModuleName, Fn, Args),
	Src = atom_to_list(Fn) ++
		  make_args(Args)  ++
		  " ->"           ++
		  Body,
	make_public_fns(T, LineNo + 1, ModuleName, NewSourceMap, [?Q(Src) | Results]).

make_args(Args) -> "(" ++ string:join(Args, ", ") ++ ")".

make_export_body(ModuleName, Fn, Args) ->
	Body = make_do_fn(ModuleName, Fn, Args) ++
	       ".",
	Body.

make_do_fn(ModuleName, Fn, Args) ->
    Hash = binary:bin_to_list(base16:encode(crypto:hash(sha, [ModuleName, atom_to_list(Fn), Args]))),
	"do_"            ++
	atom_to_list(Fn) ++
	"_"              ++
	Hash             ++
	make_args(Args).

make_exports(Exports, LineNo) ->
	ReducedExports = [{Fn, Arity} || {Fn, Arity, _Args} <- Exports],
	{[{attribute, LineNo, export, ReducedExports}], LineNo + 1}.

make_modname(ModuleName, LineNo) ->
	{[{attribute, LineNo, module, list_to_atom(ModuleName)}], LineNo + 1}.

make_line(#liffey{op = {Op, Decorator}, args = Args})  ->
	make_line(#liffey{op = Op, args = [quote(Decorator) | Args]});
make_line(#liffey{op = #'¯¯⍴¯¯'{dimensions = [1]} = Rho, args = [{var, Var, _, _}]}) ->
	NewRho = make_record(Rho),
	NewArgs = list_to_atom(Var),
	make_record(#liffey{op = NewRho, args = NewArgs});
make_line(#liffey{op = #'¯¯⍴¯¯'{} = Rho, args = Args}) ->
	NewRho = make_record(Rho),
	NewArgs = [maybe_make_record(X) || X <- Args],
	make_record(#liffey{op = NewRho, args = NewArgs});
make_line(#liffey{op = 'let', args = [Var, #liffey{op = #'¯¯⍴¯¯'{}, args = Args} | []]}) ->
	% strip the variable name and rename the op
	Src = atom_to_list(Var)  ++
	       " = ["            ++
	       expand_args(Args) ++
	       "]",
	Src;
make_line(#liffey{op = Op, args = Args}) ->
    ExpFun = fun(A, Acc) ->
				L = make_line(A),
				[L | Acc]
			 end,
    ExpArgs = lists:foldl(ExpFun, ?EMPTY_RESULTS, Args),
	Src = "pometo_runtime:"                         ++
		  atom_to_list(Op)                          ++
		  "(["                                      ++
		  string:join(lists:reverse(ExpArgs), ", ") ++
		  "])",
	Src;
make_line(X) ->
	X.

maybe_make_record(T) when is_tuple(T) -> make_record(T);
maybe_make_record(A) when is_atom(A)  -> atom_to_list(A);
maybe_make_record(X)                  -> X.

make_record({liffey, Op, Args, LineNo, CharNo}) ->

	SrcArgs = case is_atom(Args) of
		true  -> "args = "          ++
		         atom_to_list(Args) ++
		         ", ";
		false -> "args = ["        ++
				 expand_args(Args) ++
	             "], "
	end,
	"#liffey{"            ++
	"op = "               ++
	maybe_make_record(Op) ++
	", "                  ++
	SrcArgs               ++
	"line_no = "          ++
	make_line_no(LineNo)  ++
	", "                  ++
	"char_no = "          ++
	make_char_no(CharNo)  ++
	"}";
make_record({'¯¯⍴¯¯', Style, Indexed, Dims, LineNo, CharNo}) ->
	"#'¯¯⍴¯¯'{"           ++
	"style = "            ++
	atom_to_list(Style)   ++
	", "                  ++
	"indexed = "          ++
	atom_to_list(Indexed) ++
	", "                  ++
	"dimensions = ["      ++
	make_dimensions(Dims) ++
	"], "                 ++
	"line_no = "          ++
	make_line_no(LineNo)  ++
	", "                  ++
	"char_no = "          ++
	make_char_no(CharNo)  ++
	"}";
make_record({var, V, _LineNo, _CharNo}) ->
	V.

expand_args(Args) ->
	string:join([expand_arg(A) || A <- Args], ", ").

expand_arg(N) when is_integer(N) -> integer_to_list(N);
expand_arg(F) when is_float(F)   -> float_to_list(F);
expand_arg(A) when is_atom(A)    -> atom_to_list(A);
expand_arg(T) when is_tuple(T)   -> make_record(T);
expand_arg(L) when is_list(L)    -> L.

make_dimensions(Dimensions) ->
	string:join([integer_to_list(D) || D <- Dimensions], ", ").

make_line_no(none)                 -> "none";
make_line_no(N) when is_integer(N) -> integer_to_list(N).

make_char_no(none)                 -> "none";
make_char_no(N) when is_integer(N) -> integer_to_list(N).

quote(X) -> "\"" ++ X ++ "\"".

make_source_map(#liffey{op = Op, line_no = LNo, char_no = CharNo}, LineNo, SourceMap) ->
	Desc = make_desc(Op),
	SM = #sourcemap{pometo_line_no = LNo,
					pometo_char_no = CharNo,
					description    = Desc},
	SourceMap#{LineNo => SM}.

make_desc({Op, Attr}) -> atom_to_list(Op) ++ "_" ++ Attr;
make_desc(Op)         -> atom_to_list(Op).

