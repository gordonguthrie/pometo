-define(EMPTY_ACCUMULATOR,   []).
-define(EMPTY_MAP,           #{}).
-define(SPACE,               32).
-define(START_COUNTING_ARGS, 1).

-define(shp(Dim),         #'$shape¯'{dimensions = Dim}).
-define(shape(Dim, Type), #'$shape¯'{dimensions = Dim,
									 									 type       = Type}).
-define(complex_el(Args), #'$ast¯'{do   = #'$shape¯'{dimensions = 0,
																										 type       = complex},
																	 args = #'$ast¯'{do   = complex,
																									 args = Args}}).
-define(cmplx(Args), #'$ast¯'{do   = complex,
															args = Args}).

%% ranking records
-record(rank_fns, {optional_LHS = none,
									 iteration_fn        :: Fun,
									 inner_fn     = none :: none | Fun
									}).

%% formatting records
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
