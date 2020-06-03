-module(pometo_runtime).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

%% things exported for runtime
-export([
		  rho/1,
		  run_ast/1,
		  format/1,
		  format_errors/1
		]).

%% exported for the interpreter
-export([
		  substitute_bindings/2,
		  combine_bindings/3
		]).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
		dyadic/1,
		monadic/1,
		'let'/1
		]).

-define(EMPTY_ACCUMULATOR, []).
-define(SPACE, 32).

%%
%% Runtime API
%%

run_ast(#liffey{op = #'¯¯⍴¯¯'{}} = L)                          -> L;
run_ast(#liffey{op = 'let',         args = [_V, _A | []]} = L) -> 'let'([L]);
run_ast(#liffey{op = {dyadic, Op},  args = [A1, A2]})          -> dyadic([Op, A1, A2]);
run_ast(#liffey{op = {monadic, Op}, args = [A]})               -> monadic([Op, A]).

rho(List) when is_list(List) ->
	Len = length(List),
	#'¯¯⍴¯¯'{style      = eager,
	         indexed    = false,
	         dimensions = [Len]}.

format([]) -> [];
format(#liffey{op = #'¯¯⍴¯¯'{}, args = Args}) ->
	lists:flatten(string:join([fmt(X) || X <- Args], [" "])).

format_errors(Errors) ->
	FormattedEs = [format_error(X) || X <- Errors],
	lists:flatten(string:join(FormattedEs, "\n") ++ "\n").

%%
%% Binding substition for the interpreter
%%

substitute_bindings(Ast, Bindings) ->
	?debugFmt("Ast is ~p~n", [Ast]),
	?debugFmt("Bindings is ~p~n", [Bindings]),
	Ast.

combine_bindings(Bindings, NewBindings, Expr) ->
	?debugFmt("runtime: bindings is ~p~nnewbindings is ~p~n", [Bindings, NewBindings]),
	MergeFn = fun({K, V}, Bs) ->
		?debugFmt("in mergefn with ~p-~p for ~p~n", [K, V, Bs]),
		case maps:is_key(K, Bs) of
			false ->
				maps:put(K, #binding{expression = Expr, results = V}, Bs);
			true ->
				?debugFmt("runtime: true K is ~p~n-Bs is ~p~n", [K, Bs]),
				throw(#error{type    = "VARIABLE REDEFINITION",
					         msg1    = K,
					         msg2    = "already exists",
					         at_line = 99,
					         at_char = 99})
			end
		end,
    lists:foldl(MergeFn, Bindings, NewBindings).

%%
%% Exported for use in compiled modules
%%

'let'([#liffey{op = 'let', args = [_Var, Arg | []]}]) -> Arg.

dyadic([Op, #liffey{op = #'¯¯⍴¯¯'{dimensions = N}, args = A1} = L1,
	        #liffey{op = #'¯¯⍴¯¯'{dimensions = N}, args = A2}]) ->
		Vals = zip(A1, A2, Op, ?EMPTY_ACCUMULATOR),
		L1#liffey{args = Vals};
dyadic([Op, #liffey{op = #'¯¯⍴¯¯'{dimensions = [1]}, args = [A1]},
	        #liffey{                                 args = A2} = L2]) ->
		% order of A2 and A1 swapped and return record based on 2nd rho
		Vals = apply(A2, A1, left, Op, ?EMPTY_ACCUMULATOR),
		L2#liffey{args = Vals};
dyadic([Op, #liffey{                                 args = A1} = L1,
	        #liffey{op = #'¯¯⍴¯¯'{dimensions = [1]}, args = [A2]}]) ->
		Vals = apply(A1, A2, right, Op, ?EMPTY_ACCUMULATOR),
		L1#liffey{args = Vals}.

monadic([Op, #liffey{args = A} = L]) ->
	NewA = [execute_monadic(Op, X) || X <- A],
	L#liffey{args = NewA}.

%%
%% Helper functions
%%

zip([], [], _, Acc) -> lists:reverse(Acc);
zip([H1 | T1], [H2 | T2], Fn, Acc) ->
	NewAcc = execute_dyadic(Fn, H1, H2),
	zip(T1, T2, Fn, [NewAcc | Acc]).

apply([], _, _Direction, _Fn, Acc) -> lists:reverse(Acc);
apply([H | T], V, left, Fn, Acc) ->
	NewAcc = execute_dyadic(Fn, V, H),
	apply(T, V, left, Fn, [NewAcc | Acc]);
apply([H | T], V, right, Fn, Acc) ->
	NewAcc = execute_dyadic(Fn, H, V),
	apply(T, V, right, Fn, [NewAcc | Acc]).

execute_dyadic("+", L, R) -> L + R;
execute_dyadic("-", L, R) -> L - R;
execute_dyadic("×", L, R) -> L * R;
execute_dyadic("÷", L, R) -> L / R.

execute_monadic("+", V) -> V; % complex conjugate stub. return identity.
execute_monadic("-", V) -> -1 * V;
execute_monadic("×", V) -> signum(V); % when complex numbers are introduced, this becomes {⍵÷|⍵}.
execute_monadic("÷", V) -> 1 / V.

signum(V) when V < 0 ->
    -1;
signum(V) when V == 0 ->
    0;
signum(V) when V > 0 ->
    1.

fmt(X) when X < 0 -> io_lib:format("¯~p", [abs(X)]);
fmt(X)            -> io_lib:format("~p",  [X]).

format_error(#error{type    = T,
					msg1    = M1,
					msg2    = M2,
					expr    = E,
					at_line = AtL,
					at_char = AtC}) ->
	Pointer = case AtC of
		999999 -> "^";
		_      -> lists:flatten(lists:duplicate(AtC - 1, "-") ++ "^")
	end,
	io_lib:format("Error~n~ts~n~s~n~s (~s:~ts) on line ~p at character ~p~n",
				  [E, Pointer, T, M1, M2, AtL, AtC]).
