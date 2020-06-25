-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("errors.hrl").

-define(EMPTYACC,    []).
-define(EMPTYERRORS, []).
-define(PAD, {pad, "", 1, ""}).

-define(UPPERCASE_A_ASCII, 65).
-define(UPPERCASE_Z_ASCII, 90).

%% this interface returns the token stream with whitespace removed as
%% a convenience for lexer testing only
%% post_processing also accumulates the position of the token in the line to give
%% correct line positioning
get_tokens_TEST(X) ->
  Toks = lex(X),
  post_process(Toks, 1, X,  ?EMPTYERRORS, ?EMPTYACC).

lex(String) ->
  {ok, Toks, _} = string(String),
  Toks2 = collect_complex_nos(Toks, ?EMPTYACC),
  Toks3 = collect_variables(Toks2, ?EMPTYACC),
  Toks3.

collect_variables([], Acc) ->
  lists:reverse(Acc);
collect_variables([{j,        "J", NoChars1, _},
                   {FragType, Ch2, NoChars2, _} | T], Acc) when FragType == j             orelse
                                                                FragType == maybe_varfrag orelse
                                                                FragType == int           orelse
                                                                FragType == var ->
  NewVar = {var, "J" ++ Ch2, NoChars1 + NoChars2, "J" ++ Ch2},
  % throw this var on the loop again
  collect_variables([NewVar | T], Acc);
% Catches variables starting _Jblah
collect_variables([{maybe_varfrag, Ch1, NoChars1, _} = F,
                   {j,             "J", NoChars2, _} | T], Acc) ->
  case Ch1 of
    "_" ->
      NewVar = {var, Ch1 ++ "J", NoChars1 + NoChars2, Ch1 ++ "J"},
      % throw this var on the loop again
      collect_variables([NewVar | T], Acc);
    _ ->
      collect_variables(T, [F | Acc])
  end;
% catches variables starting with an underscore
collect_variables([{maybe_varfrag, Ch1, NoChars1, _} = F,
                   {FragType,      Ch2, NoChars2, _} = F2| T], Acc) when FragType == j orelse
                                                                    FragType == maybe_varfrag ->
  case Ch1 of
    [$_, A | _Rest] when A >= ?UPPERCASE_A_ASCII andalso
                         A =< ?UPPERCASE_Z_ASCII ->
      NewVar = {var, Ch1 ++ Ch2, NoChars1 + NoChars2, Ch1 ++ Ch2},
      % throw this var on the loop again
      collect_variables([NewVar | T], Acc);
    _ ->
      collect_variables([F2 | T], [F | Acc])
  end;
collect_variables([{var,      Ch1, NoChars1, _},
                   {FragType, Ch2, NoChars2, _} | T], Acc) when FragType == j             orelse
                                                                FragType == maybe_varfrag orelse
                                                                FragType == int ->
  NewVar = {var, Ch1 ++ Ch2, NoChars1 + NoChars2, Ch1 ++ Ch2},
  % throw this var on the loop again
  collect_variables([NewVar | T], Acc);
collect_variables([H | T], Acc) ->
  collect_variables(T, [H | Acc]).

% if the complex number has a negative sign it is deffo complex
% because variable names can't contain '¯'
collect_complex_nos([], Acc) ->
  lists:reverse(Acc);
collect_complex_nos([{unary_negate, _, NoChars1, _},
                     {Type1,        _, NoChars2, _} = R,
                     {j,            J, NoChars3, _},
                     {unary_negate, _, NoChars4, _},
                     {Type2,        _, NoChars5, _} = I| T], Acc) when (Type1 == int orelse
                                                                        Type1 == float) andalso
                                                                       (Type2 == int orelse
                                                                        Type2 == float) ->
  {Str1, RNum1} = negate(R),
  {Str2, INum2} = negate(I),
  Chars = Str1 ++ J ++ Str2,
  NoChars = NoChars1 + NoChars2 + NoChars3 + NoChars4 + NoChars5,
  NewA = {complex_number, Chars, NoChars, {RNum1, INum2}},
  collect_complex_nos(T, [NewA | Acc]);
collect_complex_nos([{Type1,        _, NoChars1, _} = R,
                     {j,            J, NoChars2, _},
                     {unary_negate, _, NoChars3, _},
                     {Type2,        _, NoChars4, _} = I | T], Acc) when (Type1 == int   orelse
                                                                         Type1 == float) andalso
                                                                        (Type2 == int    orelse
                                                                         Type2 == float) ->
  {Str1, RNum1} = identity(R),
  {Str2, INum2} = negate(I),
  Chars = Str1 ++ J ++ Str2,
  NoChars = NoChars1 + NoChars2 + NoChars3 + NoChars4,
  NewA = {complex_number, Chars, NoChars, {RNum1, INum2}},
  collect_complex_nos(T, [NewA | Acc]);
collect_complex_nos([{unary_negate, _, NoChars1, _},
                     {Type1,        _, NoChars2, _} = R,
                     {j,            J, NoChars3, _},
                     {Type2,        _, NoChars4, _} = I | T], Acc) when (Type1 == int    orelse
                                                                         Type1 == float) andalso
                                                                        (Type2 == int    orelse
                                                                         Type2 == float) ->
  {Str1, RNum1} = negate(R),
  {Str2, INum2} = identity(I),
  Chars = Str1 ++ J ++ Str2,
  NoChars = NoChars1 + NoChars2 + NoChars3 + NoChars4,
  NewA = {complex_number, Chars, NoChars, {RNum1, INum2}},
  collect_complex_nos(T, [NewA | Acc]);
% we don't yet know if this is complex because it could be part of a variable name
collect_complex_nos([{Type1, _, NoChars1, _} = R,
                     {j,     J, NoChars2, _},
                     {Type2, _, NoChars3, _} = I | T], Acc) when (Type1 == int    orelse
                                                                  Type1 == float) andalso
                                                                 (Type2 == int    orelse
                                                                  Type2 == float) ->
  {Str1, RNum1} = identity(R),
  {Str2, INum2} = identity(I),
  Chars = Str1 ++ J ++ Str2,
  NoChars = NoChars1 + NoChars2 + NoChars3,
  NewA = {maybe_complex_number, Chars, NoChars, {RNum1, INum2}},
  collect_complex_nos(T, [NewA | Acc]);
collect_complex_nos([H | T], Acc) ->
  collect_complex_nos(T, [H | Acc]).

negate({_Type, Chars, _LineNo, Val}) ->
  {"¯" ++ Chars, -Val}.

identity({_Type, Chars, _LineNo, Val}) ->
  {Chars, Val}.

type({scalar_fn, "-", _, _}) -> unary_negate;
type({int,        _,  _, _}) -> num;
type({float,      _,  _, _}) -> num;
type({Type,       _,  _, _}) -> Type.

post_process(List, _N, _Expr, [],     Acc) when List == [] orelse hd(List) == {'$end'} ->
  {ok, lists:reverse(Acc)};
post_process(List, _N, _Expr, Errors, _Acc) when List == [] orelse hd(List) == {'$end'} ->
  {error, pometo_runtime_format:format_errors(lists:reverse(Errors))};
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
