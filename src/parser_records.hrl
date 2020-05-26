%% basic AST record
-record(liffey, {
                 op,
                 args = []
                }).

%% special operator record
-record('¯¯⍴¯¯', {
                  style      = eager, % [eager | lazy]
                  indexed    = false,
                  dimensions = []
                 }).

-record(error, {
                type,
                msg1,
                msg2,
                expr,
                at_line,
                at_char
               }).

% leaf records
-record(var, {
              name
             }).

% complex number record TBD