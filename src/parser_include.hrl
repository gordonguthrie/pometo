-include_lib("eunit/include/eunit.hrl").
-include("parser_records.hrl").

append_scalar({Expr1, #'¯¯⍴¯¯'{dimensions = [D1], vals = V1, type = Type}},
              {Expr2, #'¯¯⍴¯¯'{dimensions = [D2], vals = V2, type = Type}}) ->
  % ?debugFmt("Rho1 is ~p~nRho2 is ~p~n", [Rho1, Rho2]),
  NewE = string:join([Expr1, Expr2], " "),
  io:format("Expr1 is ~p Expr2 is ~p NewE is ~p~n", [Expr1, Expr2, NewE]),
  Rho = #'¯¯⍴¯¯'{dimensions = [D1 + D2], vals = V1 ++ V2, type = Type},
  {NewE, Rho}.

tag_scalar(Type, positive, {_, _, _, _, Val}) ->
  Rho = #'¯¯⍴¯¯'{dimensions = [1],
                 type = Type,
                 vals = [Val]},
  Expr = format(Val),
  io:format("in tag scalar Val is ~p Expr is ~p~n", [Val, Expr]),
  {Expr, Rho};
tag_scalar(Type, negative, {_, _, _, _, Val}) ->
  Rho = #'¯¯⍴¯¯'{dimensions = [1],
                 type = Type,
                 vals = [-Val]},
  Expr = "¯" ++ format(Val),
  {Expr, Rho}.

extract(monadic, {scalar_fn, _, _, _, Fnname}, [{Expr, Args}]) ->
  #expr{type        = scalar,
        application = monadic,
        expression  = Fnname ++ " " ++ Expr,
        fn_name     = Fnname,
        args        = [Args]}; % args oughta be a list her
extract(dyadic, {scalar_fn, _, _, _, Fnname}, Args) ->
  {[Expr1, Expr2], Args2} = lists:unzip(Args),
  #expr{type        = scalar,
	      application = dyadic,
        expression  = Expr1 ++ " " ++ Fnname ++ " " ++ Expr2,
        fn_name     = Fnname,
        args        = Args2}.

make_let({var, _, _, _, Var} = Let, {Exprs, Vals}) ->
  io:format("in make_let for ~p~n- with ~p~n- and ~p~n", [Let, Exprs, Vals]),
  #let_op{var        = Var,
          expression = Var ++ " ← " ++ Exprs,
          vals       = Vals}.

format(X) -> lists:flatten(io_lib:format("~p", [X])).