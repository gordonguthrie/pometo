-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("errors.hrl").

-define(EMPTYACC,    []).
-define(EMPTYERRORS, []).

get_tokens(X) ->
    Toks = lex(X),
    post_process(Toks, 1, X, ?EMPTYERRORS, ?EMPTYACC).

lex(String) ->
  {ok, Toks, _} = string(String),
  Toks.

post_process(List, _N, _Expr, [],     Acc) when List == [] orelse hd(List) == {'$end'} ->
  {ok, lists:reverse(Acc)};
post_process(List, _N, _Expr, Errors, _Acc) when List == [] orelse hd(List) == {'$end'} ->
  {error, pometo_runtime:format_errors(lists:reverse(Errors))};
post_process([{invalid_token, Chars, TokenLen, _}| T], N, Expr, Errors, Acc) ->
  NewError = #error{type = 'SYNTAX ERROR', msg1 = "invalid token", msg2 = Chars, expr = Expr, at_line = scope_dictionary:get_line_no(), at_char = N},
  NewN = N + TokenLen,
  post_process(T, NewN, Expr, [NewError | Errors], Acc);
post_process([{whitespace, _Chars, TokenLen, _} | T], N, Expr, Errors,   Acc) ->
  NewN = N + TokenLen,
  post_process(T, NewN, Expr, Errors, Acc);
post_process([{Type, Chars, TokenLen, Op} | T], N, Expr, Errors, Acc) ->
  NewN = N + TokenLen,
  NewAcc = {Type, N, Chars, Op},
  post_process(T, NewN, Expr, Errors, [NewAcc | Acc]).

%% Turn .1/0.1/.1e+10/0.1e+10 into a float.

make_float(TokenChars) ->
    FloatAsStr = case lists:member($., TokenChars) of
                     true  -> TokenChars;
                     false -> add_decimal(TokenChars)
                 end,
    to_f(FloatAsStr).

add_decimal([]) -> ".0";
add_decimal([E | Tail]) when E == $e; E == $E -> ".0e" ++ Tail;
add_decimal([X | Tail]) -> [X | add_decimal(Tail)].


%% String -> integer.
to_i(Str) when is_list(Str) ->
    case to_num(Str) of
        {error, nan} -> {error, nan};
        Num          -> trunc(Num)
    end;
%% Number -> integer.
to_i(Num) when is_number(Num) -> trunc(Num).

%% String -> number.
to_num(Atom) when is_atom(Atom) ->
    to_num(atom_to_list(Atom));
to_num(Str) when is_list(Str)   ->
    Str2 = string:strip(Str),
    try conv_to_int(Str2)
    catch
        exit : _ ->
            try to_f(Str2)
            catch
                error:_ -> {error, nan};
                exit:_  -> {error, nan}
            end
    end;

to_num(Num) when is_number(Num) ->   Num.

conv_to_int(Str) when is_list(Str) ->
    try list_to_integer(Str)
    catch
        error:_ ->
            case string:tokens(Str, "e+") of
                [Int, Exp] ->
                    I2 = to_num(Int),
                    E2 = to_num(Exp),
                    case {I2, E2} of
                        {{error, nan}, _} -> exit("not integer");
                        {_, {error, nan}} -> exit("not integer");
                        {I3, E3}          -> I3 * math:pow(10, E3)
                    end;
                _          -> exit("not integer")
            end
    end.

%% @doc Convert value to float.
to_f(Str) when is_list(Str) ->
    %
    NStr = re:replace(Str, "^((?:-|\\+)?[0-9]+)(E(?:-|\\+)?[0-9]+)$", %"
                      "\\1.0\\2", [{return, list}]),
    {ok, [Val], []} = io_lib:fread("~f", NStr),
    Val;
to_f(F) when is_float(F) -> F.
