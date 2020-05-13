-record(rho, {
              style      = eager,
              indexed    = false,
              type,
              dimensions = [],
              vals       = []
             }).

-record(expr, {
    type,
    fn_name,
    args     = []
  }).