%% basic AST record
-record(liffey, {
                 op,
                 args    = [],
                 line_no = none,
                 char_no = none
                }).

%% special operator record
-record('¯¯⍴¯¯', {
                  style      = eager, % [eager | lazy]
                  indexed    = false,
                  dimensions = [],
                  line_no    = none,
                  char_no    = none
                 }).

% leaf records
-record(var, {
                name,
                line_no = none,
                char_no = none
             }).

-record(complex_no, {
                      real,
                      imaginary,
                      line_no = none,
                      char_no = none
             }).

-record(binding, {
                    expression = "",
                    results    = "",
                    line_no    = none,
                    char_no    = none
                  }).
