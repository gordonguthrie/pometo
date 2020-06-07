-module(pometo_compiler).

-export([
			compile/2,
			compile/1
		]).

-include("parser_records.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-record(sourcemap, {
					 pometo_line_no = none,
					 pometo_char_no = none,
					 description
				   }).

-define(LINE_NO_START,  1).
-define(EMPTY_RESULTS,  []).

%% fake compile point until we get full compile syntax
%% gotta call the damn module something in the meantime
compile(#liffey{} = L) -> compile(L, pometo).

% copy from https://github.com/basho/riak_ql/blob/develop/src/riak_ql_ddl_compiler.erl

compile(Functions, ModuleName) when is_list(Functions) ->

	SourceMap = #{},
	{Exports, _FunBodies} = lists:unzip(Functions),

	{ModAttr, LineNo1}  = make_modname(ModuleName, ?LINE_NO_START),
	SourceMap1 = SourceMap#{LineNo1 => #sourcemap{description = "module attribute"}},

	{ExpAttr, LineNo2}  = make_exports(Exports, LineNo1),
	SourceMap2 = SourceMap1#{LineNo2 => #sourcemap{description = "export attribute"}},

	{PublicFns,  LineNo3, SourceMap3} = make_public_fns(Exports,  LineNo2, ModuleName, SourceMap2, ?EMPTY_RESULTS),
	{PrivateFns, LineNo4, SourceMap4} = make_private_fns(Exports, LineNo3, ModuleName, SourceMap3, ?EMPTY_RESULTS),

	?debugFmt("SourceMap is ~p~n", [SourceMap4]),

	AST = ModAttr ++
		  ExpAttr ++
		  PublicFns ++
		  PrivateFns ++
		  [{eof, LineNo4
		  }],

	?debugFmt("AST is ~p~n", [AST]),

	case erl_lint:module(AST) of
        {ok, []} -> load_BEAM(AST);
        Other    -> exit(Other)
    end.

load_BEAM(AST) ->
	{ok, Mod, Bin} = compile:forms(AST),
	BeamFileName = "/tmp/" ++ atom_to_list(Mod) ++ ".beam",
    {module, Mod} = code:load_binary(Mod, BeamFileName, Bin).

make_private_fns([], LineNo, _ModuleName, SourceMap, Results) ->
	{lists:reverse(Results), LineNo, SourceMap};
make_private_fns([{Fn, Arity, Args} | T], LineNo, ModuleName, SourceMap, Results) ->
	Desc = "private function of " ++
		   atom_to_list(Fn) ++
		   "/" ++
		   integer_to_list(Arity),
	NewSourceMap = SourceMap#{LineNo => #sourcemap{description = Desc}},
	Src = make_do_fn(ModuleName, Fn, Args) ++
		  "->\n" ++
		  "\"randomette\".",
	% we have added an extra line
	make_private_fns(T, LineNo + 2, ModuleName, NewSourceMap, [?Q(Src) | Results]).

make_public_fns([], LineNo, _ModuleName, SourceMap, Results) ->
	{lists:reverse(Results), LineNo, SourceMap};
make_public_fns([{Fn, Arity, Args} | T], LineNo, ModuleName, SourceMap, Results) ->
	Desc = "public declaration of " ++
		   atom_to_list(Fn) ++
		   "/" ++
		   integer_to_list(Arity),
	NewSourceMap = SourceMap#{LineNo => #sourcemap{description = Desc}},
	{Body, NewLineNo} = make_export_body(ModuleName, Fn, Args, LineNo + 1),
	Src = atom_to_list(Fn) ++
		  make_args(Args) ++
		  "->\n" ++
		  Body,
	% we have added an extra line
	make_public_fns(T, NewLineNo + 1, ModuleName, NewSourceMap, [?Q(Src) | Results]).

make_args(Args) -> "(" ++ string:join(Args, ", ") ++ ")".

make_export_body(ModuleName, Fn, Args, LineNo) ->
	Body = make_do_fn(ModuleName, Fn, Args) ++
	       ".",
	{Body, LineNo + 1}.

make_do_fn(ModuleName, Fn, Args) ->
    Hash = binary:bin_to_list(base16:encode(crypto:hash(sha, [ModuleName, atom_to_list(Fn), Args]))),
	"do_" ++
	atom_to_list(Fn) ++
	"_" ++
	Hash ++
	make_args(Args).

make_exports(Exports, LineNo) ->
	ReducedExports = [{Fn, Arity} || {Fn, Arity, _Args} <- Exports],
	{[{attribute, LineNo, export, ReducedExports}], LineNo + 1}.

make_modname(ModuleName, LineNo) ->
	{[{attribute, LineNo, module, list_to_atom(ModuleName)}], LineNo + 1}.

% expand(#liffey{op = {Op, Decorator}, args = Args})  ->
%	expand(#liffey{op = Op, args = [Decorator | Args]});
% expand(#liffey{op = #'¯¯⍴¯¯'{} = Rho, args = Args}) ->
%	NewRho = make_tuple(Rho),
%	NewArgs = [list | [make_tuple(X) || X <- Args]],
%	liffey_to_tuple(#liffey{op = NewRho, args = NewArgs});
% expand(#liffey{op = 'let', args = [Var, Arg | []]}) ->
%	ExpArg = expand(Arg),
%	['let', [[Var, ExpArg]], Var];
% expand(#liffey{op = Op, args = Args}) ->
%  ExpArgs = [expand(A) || A <- Args],
%	[':', pometo_runtime, Op, [list | ExpArgs]];
% expand(X) -> X.

% make_tuple(T) when is_tuple(T) -> NewTuple = tuple_to_list(T),
%                                  [tuple | [maybe_quote(X) || X <- NewTuple]];
% make_tuple(X)                  -> X.

% maybe_quote(A) when is_atom(A)  -> [quote, A];
% maybe_quote(T) when is_tuple(T) -> make_tuple(T);
% maybe_quote(X)                  -> X.

% liffey_to_tuple(#liffey{} = L) ->
%	[H | T] = tuple_to_list(L),
%	[tuple, [quote, H] | T].
