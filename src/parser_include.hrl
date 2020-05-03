-include_lib("eunit/include/eunit.hrl").

-record(rho, {
              style      = pure,
              type,
              dimensions = [],
              vals       = []
             }).

append_scalar(#rho{type = Type} = Rho1, #rho{type = Type} = Rho2) ->
  ?debugFmt("Rho1 is ~p~nRho2 is ~p~n", [Rho1, Rho2]),
  Rho2.

tag_scalar(Type, Val) ->
  #rho{dimensions = [0],
       type = Type,
       vals = [Val]}.
