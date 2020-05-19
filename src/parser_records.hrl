-record('¯¯⍴¯¯', {
                  style      = eager, % [eager | lazy]
                  indexed    = false,
                  type,               % int | float
                  dimensions = [],
                  vals       = []
                 }).

-record(expr, {
    type,
    expression,
    application,      % [monadic | dyadic]
    fn_name,
    args         = []
  }).

-record(let_op, {
     var,
     expression,
     vals
  }).