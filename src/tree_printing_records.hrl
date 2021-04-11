-record(printable_tree, {
                         root   = "",
                         leaves = [],
                         row,
                         col
                         }).

-record(printcell, {row,
                    col,
                    width,
                    x_offset = 0,
                    y_offset = 0,
                    text}).