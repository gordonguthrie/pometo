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

% leaf records
-record(var, {
              name,
              expr
             }).

% complex number record TBD