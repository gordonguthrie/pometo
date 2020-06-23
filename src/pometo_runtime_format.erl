-module(pometo_runtime_format).

-include("parser_records.hrl").
-include("errors.hrl").
-include("comments.hrl").
-include("runtime_include.hrl").

%% things exported for runtime
-export([
		  format/1,
		  format_errors/1,
		  make_error/5
		]).

%% runtime utility functions
-export([
		 fmt/1
		]).

%% exports for testing
-export([
		build_segments_TEST/1,
		normalise_widths_TEST/1
		]).


%% ascii art boxes in unicode
-define(TOPLEFT,        9484).
-define(TOPRIGHT,       9488).
-define(BOTTOMLEFT,     9492).
-define(BOTTOMRIGHT,    9496).
-define(VERTICALLINE,   9474).
-define(HORIZONTALLINE, 9472).

-define(STARTESEQUENCE, 1).
-define(STARTWIDTH,     0).
-define(DEFAULTBOXING,  none).

%%%
%%% API
%%%

format([]) -> [];
format(#'$ast¯'{op = #'$shape¯'{}} = AST) ->
	Frags = build_segments(AST),
	print(Frags);
format(#comment{msg = Msg,
				at_line = LNo,
				at_char = CNo}) ->
	io_lib:format("~ts on line ~p at character ~p~n", [Msg, LNo, CNo]);
format({error, Err}) ->
	format_errors([Err]).

format_errors(Errors) ->
	FormattedEs = [format_error(X) || X <- Errors],
	lists:flatten(string:join(FormattedEs, "\n") ++ "\n").

print(List) ->
	io:format("in print List is ~p~n", [List]),
	Widths = normalise_widths(List),
	io:format("in print Widths is ~p~n", [Widths]),
	Stripped = [X || #fmt_line{segs = X} <- List],
	Zipped = lists:zip(Stripped, Widths),
	Lines = [format_line(Line, LineWidths, ?EMPTY_ACCUMULATOR) || {Line, LineWidths} <- Zipped],
	io:format("Lines is ~p~n", [Lines]),
	print(Lines, ?EMPTY_ACCUMULATOR).

print([], Acc) -> string:join(lists:reverse(Acc), "\n");
print([[H] | T], Acc) ->
	io:format("H is ~p~n", [H]),
	NewAcc = string:join([io_lib:format("~ts", [X]) || X <- H], "\n"),
	print(T, [NewAcc| Acc]).

join(List) -> join2(List, ?EMPTY_MAP).

join2([],      Acc) -> {_DiscardKeys, Strings} = lists:unzip(maps:to_list(Acc)),
					   [Strings];
join2([H | T], Acc) ->
	NewAcc = join3(H, 1, Acc),
	join2(T, NewAcc).

join3([], _Key, Map) ->
	Map;
join3([[Val] | T], Key, Map) ->
	NewMap = case maps:is_key(Key, Map) of
		true  -> OldVal = maps:get(Key, Map),
				 NewVal = OldVal ++ " " ++ Val,
				 maps:put(Key, NewVal, Map);
		false -> maps:put(Key, Val, Map)
	end,
	join3(T, Key + 1, NewMap).

format_line([], _Widths, Acc) ->
	io:format("in format_line (1) returning- Acc ~p~n", [lists:reverse(Acc)]),
	join(lists:reverse(Acc));
format_line([#fmt_segment{strings = Strings,
						 is_leaf = false} = S| T], [Widths | Rest], Acc) ->
	io:format("in print line (2)~n- with S of ~p~n- Widths of ~p~n", [S, Widths]),
	#fmt_segment{width   = NW,
			     height  = H,
			     boxing  = NB,
			     strings = InnerStrings} = Widths,
	NewLines = format_line(Strings, InnerStrings, ?EMPTY_ACCUMULATOR),
	io:format("in print line (2) NewLines is ~p~n", [NewLines]),
	PaddedLines = pad_lines(NewLines, NW, H, NB),
	format_line(T, Rest, [PaddedLines | Acc]);
format_line([#fmt_segment{strings = [S],
						 is_leaf = true} | T], [Widths | Rest], Acc) ->
	io:format("in print line (3)~n- with S of ~p~n- Widths of ~p~n", [S, Widths]),
	#fmt_segment{width  = NW,
			     height = H,
			     boxing = NB} = Widths,
	Line = [pad_width(S, NW)],
	PaddedLines = pad_lines([Line], NW, H, NB),
	format_line(T, Rest, [PaddedLines | Acc]).

pad_lines(Lines, Width, Height, Boxing) ->
	io:format("in pad line Lines is ~p Width is ~p Height is ~p Boxing is ~p~n", [Lines, Width, Height, Boxing]),
	[io:format("Line in Lines ~p~n", [X]) || X <- Lines],
	Boxed = case Boxing of
		none       -> Lines;
		boxed      -> FinalLines = side_pad(Lines, [?VERTICALLINE]),
					  Border     = lists:duplicate(Width - 2, ?HORIZONTALLINE),
				 	  Top    = [[?TOPLEFT]    ++ Border ++ [?TOPRIGHT]],
				 	  Bottom = [[?BOTTOMLEFT] ++ Border ++ [?BOTTOMRIGHT]],
				 	  [Top] ++ FinalLines ++ [Bottom];
		blankboxed -> FinalLines = side_pad(Lines, [?SPACE]),
					  Border     = [[lists:duplicate(Width + 2, ?SPACE)]],
					  Border ++ FinalLines ++ Border
	end,
	[io:format("Line in Boxed ~p~n", [X]) || X <- Boxed],
	H = length(Boxed),
	io:format("in pad line H is ~p~n", [H]),
	HPad = if
		Height - H > 0 -> Height - H;
		el/=se         -> 0
	end,
	io:format("HPad is ~p~n", [HPad]),
	PaddedLines = case HPad of
					0 -> Boxed;
					_ -> BlankLine  = [lists:duplicate(Width, ?SPACE)],
						 BlankLines = lists:duplicate(HPad,  BlankLine),
						 Boxed ++ BlankLines
				end,
	PaddedLines,
	[io:format("Line in PaddedLines ~p~n", [X]) || X <- PaddedLines],
	PaddedLines.

side_pad(List, Padding) ->
	io:format("in side padding List is ~p Padding is ~p~n", [List, Padding]),
	Return = [[Padding ++ X ++ Padding] || [X] <- List],
	io:format("side pad returns ~p~n", [Return]),
	Return.

pad_width(X, W) -> Len = length(X),
				   Pad = W - Len,
				   X ++ lists:duplicate(Pad, " ").

normalise_widths_TEST(A) -> normalise_widths(A).

normalise_widths(Lines) ->
	RawSegs = [X || #fmt_line{segs = X} <- Lines],
	io:format("RawSegs is ~p~n", [RawSegs]),
	Len = length(hd(RawSegs)),
	Blank = lists:duplicate(Len, 0),
	ConsolidateFn = fun(Line, {IsBlankBoxed, Height, Acc}) ->
		io:format("in consolidate fn Line is ~p~n", [Line]),
		_NewWidest = normalise_segs(Line, Acc, IsBlankBoxed, Height, ?EMPTY_ACCUMULATOR)
	end,
	{ForceBox, NormalisedHeight, NormalisedWidths} = lists:foldl(ConsolidateFn, {false, 0, Blank}, RawSegs),
	io:format("ForceBox is ~p NormalisedHeight is ~p NormalisedWidths is ~p~n", [ForceBox, NormalisedHeight, NormalisedWidths]),
	ApplyFun = fun(Line) ->
		apply_normal(Line, NormalisedWidths, NormalisedHeight, ForceBox, ?EMPTY_ACCUMULATOR)
	end,
	RenormalisedWidths = lists:map(ApplyFun, RawSegs),
	io:format("NormalisedWidths is ~p~n", [RenormalisedWidths]),
	RenormalisedWidths.

apply_normal([], _Height, _ForceBlank, _Normalised, Acc) ->
	lists:reverse(Acc);
apply_normal([#fmt_segment{boxing = B1} = Seg | T], [Width | Rest], Height, ForceBlank, Acc) ->
	NewB = case {B1, ForceBlank} of
		{none, true} -> blankboxed;
		_            -> B1
	end,
	apply_normal(T, Rest, Height, ForceBlank, [Seg#fmt_segment{width  = Width,
													   		   height = Height,
												  	   		   boxing = NewB} | Acc]).

normalise_segs([], [], IsBlankBoxed, Height, Acc) -> {IsBlankBoxed, Height, lists:reverse(Acc)};
normalise_segs([Line | T1], [Width | T2], IsBlankBoxed, Height, Acc) ->
	#fmt_segment{width  = W1,
				 height = H1,
				 boxing = B1} = Line,
	NewW = get_greater(W1, Width),
	NewH = get_greater(H1, Height),
	NewBoxing = case B1 of
					none       -> IsBlankBoxed;
					boxed      -> true;
					blankboxed -> true
				end,
	normalise_segs(T1, T2, NewBoxing, NewH, [NewW | Acc]).

get_greater(A, B) when A > B -> A;
get_greater(_, B)            -> B.


build_segments_TEST(A) -> build_segments(A).

build_segments(#'$ast¯'{op = #'$shape¯'{dimensions = 0},
	                     args = Arg}) ->
	_SizedLines = [#fmt_line{segs = size_line(0, [Arg])}];
build_segments(#'$ast¯'{op = #'$shape¯'{dimensions = D},
	                     args = Args}) ->
	[LineSize | Dims] = lists:reverse(D),
	Lines = make_lines(Args, LineSize, ?EMPTY_ACCUMULATOR),
	SplitFn = fun(Ls) ->
		NewSegs   = split_line(Dims, Ls),
		SizedSegs = size_line(length(Dims), NewSegs),
		#fmt_line{segs = SizedSegs}
	end,
	lists:map(SplitFn, Lines).

make_lines([], _N, Acc) -> lists:reverse(Acc);
make_lines(List, N, Acc) ->
	{Chunk, Rest} = lists:split(N, List),
	make_lines(Rest, N, [Chunk | Acc]).

size_line(0, Args) ->
	% we don't reverse because we are not reversing the foldl
	SizeFn = fun(X, Acc) ->
		[fmt(X) | Acc]
	end,
	lists:foldl(SizeFn, [], Args);
size_line(N, Args) -> lists:flatten([size_line(N - 1, X) || X <- Args]).

split_line([], Args) ->
	lists:reverse(Args);
split_line([H | T], Args) ->
	Len = length(Args),
	Slice = trunc(Len/H),
	NewArgs = split_l2(Args, Slice, ?EMPTY_ACCUMULATOR),
	[split_line(T, X) || X <- NewArgs].

split_l2([],   _N, Acc) -> lists:reverse(Acc);
split_l2(List, N,  Acc) -> {First, Rest} = lists:split(N, List),
						   split_l2(Rest, N, [First | Acc]).

fmt(#'$ast¯'{op   = complex,
	         args = [R, I]}) when R < 0 andalso
                                  I < 0 -> make_frag("¯~pJ¯~p", [abs(R), abs(I)]);
fmt(#'$ast¯'{op   = complex,
	         args = [R, I]}) when R < 0 -> make_frag("¯~pJ~p",  [abs(R), abs(I)]);
fmt(#'$ast¯'{op   = complex,
	         args = [R, I]}) when I < 0 -> make_frag("~pJ¯~p",  [abs(R), abs(I)]);
fmt(#'$ast¯'{op   = complex,
	         args = [R, I]})            -> make_frag("~pJ~p",   [abs(R), abs(I)]);
fmt(#'$ast¯'{} = A)                     -> [#fmt_line{segs = Strings}] = build_segments(A),
										   {Width, Height} = get_size(Strings),
										   #fmt_segment{strings = Strings,
										   			    width   = Width  + 2,
										   			    height  = Height + 2,
										                boxing  = boxed};
fmt(X)                       when X < 0 -> make_frag("¯~p", [abs(X)]);
fmt(X)                                  -> make_frag("~p",  [X]).

get_size(List) ->
	Padding = length(List) - 1,
	{W, H} = get_size2(List, 0, 0),
	{W + Padding, H}.

get_size2([],                             Width, Height) -> {Width, Height};
get_size2([#fmt_segment{width  = W,
						height = H} | T], Width, Height) -> NewH = get_greater(Height, H),
															get_size2(T, Width + W, NewH).

make_frag(Text, Args) ->
	String = lists:flatten(io_lib:format(Text, Args)),
	Width = length(String),
	#fmt_segment{strings = [String],
	             width   = Width,
	             is_leaf = true}.

make_error(Type, Msg1, Msg2, LineNo, CharNo) when is_list(Type)      andalso
												  is_list(Msg1)      andalso
												  is_list(Msg2)      andalso
												  is_integer(LineNo) andalso
												  is_integer(CharNo) ->
	#error{
	    	type = Type,
			msg1 = Msg1,
			msg2 = Msg2,
			expr = "",
			at_line = LineNo,
			at_char = CharNo
		   }.

format_error(#error{type    = T,
					msg1    = M1,
					msg2    = M2,
					expr    = E,
					at_line = AtL,
					at_char = AtC}) ->
	Pointer = case AtC of
		999999 -> "";
		none   -> "";
		_      -> lists:flatten(lists:duplicate(AtC - 1, "-") ++ "^")
	end,
	io_lib:format("Error~n~ts~n~s~n~s (~ts:~ts) on line ~p at character ~p~n",
				  [E, Pointer, T, M1, M2, AtL, AtC]).
