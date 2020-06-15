%% basic AST record
-record(ast, {
                op,
                args    = [],
                line_no = none,
                char_no = none
              }).

%% special operator record
-record('$¯¯⍴¯¯', {
                  style      = eager, % [eager | lazy]
                  indexed    = false,
                  dimensions = [],
                  line_no    = none,
                  char_no    = none
                 }).

% leaf records
-record('$¯¯var¯¯', {
                     name,
                     line_no = none,
                     char_no = none
                    }).

% this record is used in the parser for interim processing - they
% are eliminated in the creation of the AST

% therefore they should be able to be refactored out???

-record(complex, {
                   real,
                   imag
                 }).
