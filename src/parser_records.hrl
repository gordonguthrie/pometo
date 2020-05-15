-record('__⍴__', {
                  style      = eager, % [eager | lazy]
                  indexed    = false,
                  type,               % scalar_fn
                  dimensions = [],
                  vals       = []
                 }).

-record(expr, {
    type,
    application,      % [monadic | dyadic]
    fn_name,
    args         = []
  }).