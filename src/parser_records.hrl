%% basic AST record
-record('$ast¯', {
                  do,
                  args            = [],
                  line_no         = none,
                  char_no         = none
                 }).

%% special operator record
-record('$shape¯', {
                    indexed    = false,
                    dimensions = [],
                    forcing    = none,
                    type       = none, % number | boolean | mixed | array
                    line_no    = none,
                    char_no    = none
                   }).

% leaf records
-record('$var¯', {
                  name,
                  line_no = none,
                  char_no = none
                 }).
