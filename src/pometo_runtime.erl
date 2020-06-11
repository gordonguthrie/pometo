-module(pometo_runtime).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

%% things exported for runtime
-export([
		  rho/1,
		  run_ast/2,
		  format/1,
		  format_errors/1
		]).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
		dyadic/1,
		monadic/1,
		runtime_let/1
		]).

-define(EMPTY_ACCUMULATOR, []).
-define(SPACE, 32).

%%
%% Runtime API
%%

run_ast(AST, Str) ->
	try run_ast2(AST)
	catch
		throw:E -> {error, #error{} = Err} = E,
		           {error, Err#error{expr = Str}}
	end.

run_ast2(#liffey{op   = #'¯¯⍴¯¯'{}} = L)   -> L;
run_ast2(#liffey{op   = 'let',
	             args = [_V, A | []]} = L) -> NewL = L#liffey{op   = runtime_let,
                                                              args = A},
                                              runtime_let([NewL]);
run_ast2(#liffey{op   = {dyadic, Op},
	             args = [A1, A2]})         -> dyadic([Op, A1, A2]);
run_ast2(#liffey{op   = {monadic, Op},
	             args = [A]})              -> monadic([Op, A]).

rho(List) when is_list(List) ->
	Len = length(List),
	#'¯¯⍴¯¯'{style      = eager,
	         indexed    = false,
	         dimensions = [Len]}.

format([]) -> [];
format(List) when is_list(List) ->
	lists:flatten(string:join([fmt(X) || X <- List], [" "]));
format(#liffey{op = #'¯¯⍴¯¯'{}, args = Args}) ->
	lists:flatten(string:join([fmt(X) || X <- Args], [" "]));
format({error, Err}) ->
	format_errors([Err]).

format_errors(Errors) ->
	FormattedEs = [format_error(X) || X <- Errors],
	lists:flatten(string:join(FormattedEs, "\n") ++ "\n").

%%
%% Exported for use in compiled modules
%%

runtime_let([#liffey{op = runtime_let, args = Args}]) -> Args.

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
		L1#liffey{args = Vals};
dyadic([Op, #liffey{op = #'¯¯⍴¯¯'{dimensions = N1}, line_no = LNo, char_no = ChNo},
	        #liffey{op = #'¯¯⍴¯¯'{dimensions = N2}}]) ->
		Msg1 = io_lib:format("dimensions mismatch in dyadic ~p", [Op]),
		Msg2 = io_lib:format("LHS dimensions ~p: RHS dimensions ~p", [N1, N2]),
		Error = #error{
		                type = "LENGTH ERROR",
						msg1 = Msg1,
						msg2 = Msg2,
						expr = "",
						at_line = LNo,
						at_char = ChNo
					   },
		throw({error, Error}).

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
execute_dyadic("÷", L, R) -> L / R;
execute_dyadic("|", 0, R) -> R;
execute_dyadic("|", L, R) -> R/L.

execute_monadic("+", V) -> V; % complex conjugate stub. return identity.
execute_monadic("-", V) -> -1 * V;
execute_monadic("×", V) -> signum(V); % when complex numbers are introduced, this becomes {⍵÷|⍵}.
execute_monadic("÷", V) -> 1 / V;
execute_monadic("|", V) -> abs(V).

signum(V) when V <  0 -> -1;
signum(V) when V == 0 -> 0;
signum(V) when V >  0 -> 1.

fmt(X) when X < 0 -> io_lib:format("¯~p", [abs(X)]);
fmt(X)            -> io_lib:format("~p",  [X]).

format_error(#error{type    = T,
					msg1    = M1,
					msg2    = M2,
					expr    = E,
					at_line = AtL,
					at_char = AtC}) ->
	Pointer = case AtC of
		999999 -> "";
		none   -> "";
		_      -> lists:flatten(lists:duplicate(AtC - 1, "-") ++ "^")
	end,
	io_lib:format("Error~n~ts~n~s~n~s (~s:~ts) on line ~p at character ~p~n",
				  [E, Pointer, T, M1, M2, AtL, AtC]).
