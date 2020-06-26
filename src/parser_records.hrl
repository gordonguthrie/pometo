%% basic AST record
-record('$ast¯', {
                  op,
                  args    = [],
                  line_no = none,
                  char_no = none
                 }).

%% special operator record
-record('$shape¯', {
                    shaping    = eager, % [eager | lazy]
                    indexed    = false,
                    dimensions = [],
                    forcing    = none,
                    type       = none,
                    line_no    = none,
                    char_no    = none
                   }).

% leaf records
-record('$var¯', {
                  name,
                  line_no = none,
                  char_no = none
                 }).
