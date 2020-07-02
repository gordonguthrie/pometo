-define(EMPTY_ACCUMULATOR,   []).
-define(EMPTY_MAP,           #{}).
-define(SPACE,               32).
-define(START_COUNTING_ARGS, 0).

-define(shp(Dim),         #'$shape¯'{dimensions = Dim}).
-define(shape(Dim, Type), #'$shape¯'{dimensions = Dim,
									 type       = Type}).
-define(complex_el(Args), #'$ast¯'{op   = #'$shape¯'{dimensions = 0,
	                                                 type       = complex},
                                   args = #'$ast¯'{op   = complex,
                               			           args = Args}}).
-define(complex_no(Args), #'$ast¯'{op   = complex,
                               	   args = Args}).


-record(fmt_segment, {
					 	strings = "",
					 	width   = 0,
					 	height  = 1,
					 	boxing  = none, % none, boxed, blankboxed
					 	is_leaf = false
						}).

-record(fmt_line, {
					 segs = []
					}).

-record(box, {
				 width  = 0,
				 height = 0
			 }).
