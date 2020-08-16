%% basic AST record
-record('$ast¯', {
                  do,
                  args            = [],
                  line_no         = none :: none | Integer,
                  char_no         = none :: none | Integer
                 }).

%% special shape record
-record('$shape¯', {
                    indexed    = false :: boolean(),
                    dimensions = []    :: 0    | unsized_vector | [Integer],
                    forcing    = none  :: none | index | unindex,
                    type       = none  :: none | array | mixed | boolean | complex | number | variable | scalar | func | maybe_func | unfinalised_vector,
                    line_no    = none  :: none | Integer,
                    char_no    = none  :: none | Integer
                   }).

%% special func record
-record('$func¯', {
                    do                         :: string() | [string()],
                    type                       :: niladic | monadic | dyadic | ambivalent | make_train,
                    construction   = primitive :: primitive | operator | atop | fork_fgh | fork_Agh,
                    result         = explicit  :: none | supressed | explicit,
                    shape_changing = false     :: boolean(),
                    rank           = none      :: first | last | Integer,
                    line_no        = none      :: none | Integer,
                    char_no        = none      :: none | Integer
                   }).

%% special operator record
-record('$op¯', {op             :: string(),
                 fns     = []   :: [string()],
                 line_no = none :: none | Integer,
                 char_no = none :: none | Integer
                 }).

% leaf records
-record('$var¯', {
                  name,
                  line_no = none :: none | Integer,
                  char_no = none :: none | Integer
                 }).
