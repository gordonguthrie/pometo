-include_lib("eunit/include/eunit.hrl").
-include("parser_records.hrl").

append_scalar(#rho{dimensions = [D1], vals = V1, type = Type},
              #rho{dimensions = [D2], vals = V2, type = Type}) ->
  % ?debugFmt("Rho1 is ~p~nRho2 is ~p~n", [Rho1, Rho2]),
  #rho{dimensions = [D1 + D2], vals = V1 ++ V2, type = Type}.

tag_scalar(Type, {_, _, _, _, Val}) ->
  #rho{dimensions = [1],
       type = Type,
       vals = [Val]}.

extract({scalar_fn, _, _, _, Fnname}, Args) -> #expr{type    = scalar,
                                                     fn_name = Fnname,
                                                     args    = Args}.
