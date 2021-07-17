-record(printable_tree, {
                         root   = ""        :: string(),
                         leaves = []        :: list(),
                         row                :: integer(),
                         col                :: integer(),
                         needs_roof = false :: true | false
                         }).

-record(printcell, {row                :: integer(),
                    col                :: integer(),
                    width              :: integer(),
                    x_offset = 0       :: integer(),
                    y_offset = 0       :: integer(),
                    needs_roof = false :: initial | subsequent | last | false,
                    text               :: string()}).