-include_lib("eunit/include/eunit.hrl").
-include("parser_records.hrl").

append_scalar(#'¯¯⍴¯¯'{dimensions = [D1], vals = V1, type = Type},
              #'¯¯⍴¯¯'{dimensions = [D2], vals = V2, type = Type}) ->
  % ?debugFmt("Rho1 is ~p~nRho2 is ~p~n", [Rho1, Rho2]),
  #'¯¯⍴¯¯'{dimensions = [D1 + D2], vals = V1 ++ V2, type = Type}.

tag_scalar(Type, {_, _, _, _, Val}) ->
  #'¯¯⍴¯¯'{dimensions = [1],
           type = Type,
           vals = [Val]}.

extract(Application, {scalar_fn, _, _, _, Fnname}, Args) ->
	#expr{type        = scalar,
	      application = Application,
        fn_name     = Fnname,
        args        = Args}.

make_let({var, _, _, _, Var}, Vals) ->
  io:format("in make let for ~p and ~p~n", [Var, Vals]),
  #let_op{var  = Var,
          vals = Vals}.
