-module(pometo_runtime_format).

-include_lib("eunit/include/eunit.hrl").

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
		  build_segments_TEST/1
		]).

%% the size we allow the formated output to be
-define(DISPLAYWIDTH,  80).
-define(DISPLAYHEIGHT, 40).

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
format(#'$ast¯'{do   = #'$shape¯'{indexed = true} = Shp,
								args = Args} = AST) ->
	% if it is indexed we just unindex it before display
	NewArgs = pometo_runtime:unindex(Args),
	format(AST#'$ast¯'{do   = Shp#'$shape¯'{indexed = false},
									   args = NewArgs});
% special case for the null return from ⍴ on a scalar
format(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
				args = []}) ->
	"";
format(#'$ast¯'{do   = #'$shape¯'{dimensions = 0,
																  type       = array},
							  args = Args} = AST) ->
	% promote an array scalar to a mixed vector for printing
	NewDims = [length(Args)],
	format(AST#'$ast¯'{do   = #'$shape¯'{dimensions = NewDims,
								         type       = mixed}});
% scalar array first
format(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
							  args = #'$ast¯'{do = #'$shape¯'{}} = InnerAST}) ->
	format(InnerAST);
% now a normal scalar (including complex nos)
format(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
							  args = Arg}) ->
	#fmt_segment{strings = [String]} = fmt(Arg),
	String;
% if its unsized, size it and bung it back around
format(#'$ast¯'{do   = #'$shape¯'{dimensions = unsized_vector} = Shp,
								args = Args} = AST) ->
	Dims = length(Args),
	format(AST#'$ast¯'{do = Shp#'$shape¯'{dimensions = [Dims]}});
format(#'$ast¯'{do   = #'$shape¯'{dimensions = Dims},
								args = Args} = AST) when is_list(Args) ->
	Len = length(Dims),
	if
		Len <  3 -> Frags = build_segments(AST),
								Block = print(Frags),
								maybe_truncate_block(Block);
		Len >= 3 -> [First, Second | _Rest] = lists:reverse(Dims),
								Block = chunk_format(AST, First, Second, ?EMPTY_ACCUMULATOR),
								maybe_truncate_block(Block)
	end;
format(#comment{msg     = Msg,
								at_line = LNo,
								at_char = CNo}) ->
	io_lib:format("~ts on line ~p at character ~p~n", [Msg, LNo, CNo]);
format({error, Err}) ->
	format_errors([Err]);
format(X) ->
	X.

format_errors(Errors) ->
	FormattedEs = [format_error(X) || X <- Errors],
	lists:flatten(string:join(FormattedEs, "\n")).

chunk_format(#'$ast¯'{args = []}, _First, _Second, Acc) ->
	string:join(lists:reverse(Acc), "\n\n");
chunk_format(#'$ast¯'{args = Args} = AST, First, Second, Acc) ->
	Chunk = First * Second,
	{Chunked, Remainder} = lists:split(Chunk, Args),
	NewAcc = format(AST#'$ast¯'{do   = #'$shape¯'{dimensions = [Second, First]},
							    args = Chunked}),
	NewAST = AST#'$ast¯'{args = Remainder},
	chunk_format(NewAST, First, Second, [NewAcc | Acc]).

print(List) ->
	List2 = normalise_widths(List),
	List3 = normalise_heights(List2, ?EMPTY_ACCUMULATOR),
	Lines = [format_line(Line, ?EMPTY_ACCUMULATOR) || Line <- List3],
	print2(Lines, ?EMPTY_ACCUMULATOR).

print2([], Acc) ->
	_Block = string:join(lists:reverse(Acc), "\n");
print2([[H] | T], Acc) ->
	NewAcc = string:join([io_lib:format("~ts", [X]) || X <- H], "\n"),
	print2(T, [NewAcc| Acc]).

maybe_truncate_block(Block) ->
	% the length of the block is the number_of_lines + (number_of_lines - 1)
	% each line (except the last) is followed by a line return
	Len = trunc((length(Block) + 1)/2),
	_NewBlock = if
		Len >  ?DISPLAYHEIGHT -> Cut = trunc((Len - ?DISPLAYHEIGHT)/2),
								 {Keep, _Discard} = lists:split(length(Block) - Cut, Block),
								 Keep ++ ["\n[... " ++ integer_to_list(Len - ?DISPLAYHEIGHT) ++ " lines cut...]"];
		Len =< ?DISPLAYHEIGHT -> Block
	end.

join(List) -> join2(List, ?EMPTY_MAP).

join2([],      Acc) -> {_DiscardKeys, Strings} = lists:unzip(maps:to_list(Acc)),
					   [Strings];
join2([H | T], Acc) ->
	NewAcc = join3(H, 1, Acc),
	join2(T, NewAcc).

join3([], _Key, Map) ->
	Map;
join3([Val | T], Key, Map) ->
	NewMap = case maps:is_key(Key, Map) of
		true  -> OldVal = maps:get(Key, Map),
				 NewVal = lists:flatten(OldVal ++ " " ++ Val),
				 maps:put(Key, NewVal, Map);
		false -> maps:put(Key, Val, Map)
	end,
	join3(T, Key + 1, NewMap).

format_line([], Acc) ->
	[Lines] = join(lists:reverse(Acc)),
	[[maybe_truncate_line(X) || X <- Lines]];
format_line([#fmt_segment{strings = Strings,
													width   = NW,
													height  = H,
													boxing  = NB,
													is_leaf  = false} | T], Acc) ->
	[NewLines] = format_line(Strings, ?EMPTY_ACCUMULATOR),
	PaddedLines = pad_lines(NewLines, NW, H, NB),
	format_line(T, [PaddedLines | Acc]);
format_line([#fmt_segment{strings = [Strs],
													width   = NW,
													height  = H,
													boxing  = NB,
													is_leaf = true} | T], Acc) ->
	PaddedLines = pad_lines([Strs], NW, H, NB),
	format_line(T, [PaddedLines | Acc]).

maybe_truncate_line(Line) ->
	Len = length(Line),
	if
		Len >  ?DISPLAYWIDTH -> Cut = Len - ?DISPLAYWIDTH,
								Msg = " chars deleted ]",
							    CutLen = Cut + length(Msg) + 1,
							    Append = "[" ++ integer_to_list(CutLen) ++ Msg,
							    {Start, _Cut} = lists:split(?DISPLAYWIDTH - length(Append), Line),
							    Start ++ Append;
		Len =< ?DISPLAYWIDTH -> Line
	end.

pad_lines(Lines, Width, Height, Boxing) ->
	Boxed = case Boxing of
		none ->
			rectify(Lines, Width, ?EMPTY_ACCUMULATOR);
		boxed ->
			RectifiedLines = rectify(Lines, Width - 2, ?EMPTY_ACCUMULATOR),
			FinalLines = side_pad(RectifiedLines, [?VERTICALLINE], ?EMPTY_ACCUMULATOR),
			Border     = lists:duplicate(Width - 2, ?HORIZONTALLINE),
			Top        = [[?TOPLEFT]    ++ Border ++ [?TOPRIGHT]],
			Bottom     = [[?BOTTOMLEFT] ++ Border ++ [?BOTTOMRIGHT]],
			[Top] ++ FinalLines ++ [Bottom];
		blankboxed ->
			RectifiedLines = rectify(Lines, Width, ?EMPTY_ACCUMULATOR),
			Border         = [[lists:duplicate(Width, ?SPACE)]],
			Border ++ RectifiedLines ++ Border
	end,
	H = length(Boxed),
	HPad = if
		Height - H > 0 -> Height - H;
		el/=se         -> 0
	end,
	PaddedLines = case HPad of
						0 -> Boxed;
						_ -> BlankLine  = [lists:duplicate(Width, ?SPACE)],
							              	BlankLines = lists:duplicate(HPad, BlankLine),
							              	Boxed ++ BlankLines
				   end,
	PaddedLines.

rectify([], _Width, Acc) ->
	lists:reverse(Acc);
rectify([H | T], Width, Acc) ->
	W = length(H),
	Padded = if
				W < Width -> lists:duplicate(Width - W, ?SPACE) ++ H;
				el/=se    -> H
			end,
	rectify(T, Width, [Padded | Acc]).

side_pad([], _Padding, Acc) ->
	lists:reverse(Acc);
side_pad([H | T], Padding, Acc) ->
	NewAcc = lists:flatten(Padding ++ H ++ Padding),
	side_pad(T, Padding, [NewAcc | Acc]).

normalise_heights([], Acc) -> lists:reverse(Acc);
normalise_heights([H | T], Acc) ->
	GetMaxHeightFn = fun(#fmt_segment{height = Height}, Max) ->
							get_greater(Height, Max)
					end,
	MaxHeight = lists:foldl(GetMaxHeightFn, 0, H),
	SetMaxHeightFun = fun(#fmt_segment{} = F) ->
		F#fmt_segment{height = MaxHeight}
	end,
	NormalisedHeights = lists:map(SetMaxHeightFun, H),
	normalise_heights(T, [NormalisedHeights | Acc]).

normalise_widths(Lines) ->
	RawSegs = [X || #fmt_line{segs = X} <- Lines],
	Len = length(hd(RawSegs)),
	Blank = lists:duplicate(Len, 0),
	ConsolidateFn = fun(Line, {IsBlankBoxed, Acc}) ->
		_NewWidest = normalise_segs(Line, Acc, IsBlankBoxed, ?EMPTY_ACCUMULATOR)
	end,
	{ForceBox, NormalisedWidths} = lists:foldl(ConsolidateFn, {false, Blank}, RawSegs),
	ApplyFun = fun(Line) ->
		apply_normal(Line, NormalisedWidths, ForceBox, ?EMPTY_ACCUMULATOR)
	end,
	RenormalisedWidths = lists:map(ApplyFun, RawSegs),
	RenormalisedWidths.

apply_normal([], _ForceBlank, _Normalised, Acc) ->
	lists:reverse(Acc);
apply_normal([#fmt_segment{boxing = B1} = Seg | T], [Width | Rest], ForceBlank, Acc) ->
	NewB = case {B1, ForceBlank} of
		{none, true} -> blankboxed;
		_            -> B1
	end,
	apply_normal(T, Rest, ForceBlank, [Seg#fmt_segment{width  = Width,
																										 boxing = NewB} | Acc]).

normalise_segs([], [], IsBlankBoxed, Acc) -> {IsBlankBoxed, lists:reverse(Acc)};
normalise_segs([Line | T1], [Width | T2], IsBlankBoxed, Acc) ->
	#fmt_segment{width  = W1,
							 boxing = B1} = Line,
	%% it is only now that we know if we have to pad this line with a blank box
	ActualWidth = case {B1, IsBlankBoxed} of
					{none, true} -> W1 + 2;
					_            -> W1
	end,
	NewBoxing = case B1 of
					none       -> IsBlankBoxed;
					boxed      -> true;
					blankboxed -> true
				end,
	NewW = get_greater(ActualWidth, Width),
	normalise_segs(T1, T2, NewBoxing, [NewW | Acc]).

get_greater(A, B) when A > B -> A;
get_greater(_, B)            -> B.


build_segments_TEST(A) -> build_segments(A).

build_segments(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
												args = null}) ->
	_SizedLines = [#fmt_line{segs = size_line(0, "")}];
% now handle the scalar array
build_segments(#'$ast¯'{do   = #'$shape¯'{dimensions = 0,
																					type       = array},
												args = Args}) ->
	_SizedLines = [#fmt_line{segs = size_line(0, Args)}];
build_segments(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
												args = Arg}) ->
	_SizedLines = [#fmt_line{segs = size_line(0, [Arg])}];
build_segments(#'$ast¯'{do   = #'$shape¯'{dimensions = D},
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
	Lines = lists:foldl(SizeFn, [], Args),
	MaxHeightFn = fun(#fmt_segment{height = H}, Acc) ->
					if
						H >  Acc -> H;
						H =< Acc -> Acc
					end
				   end,
	MaxHeight = lists:foldl(MaxHeightFn, 0, Lines),
	NormaliseHeightFn = fun(#fmt_segment{} = Seg) ->
							Seg#fmt_segment{height = MaxHeight}
						end,
	Normalised = lists:map(NormaliseHeightFn, Lines),
	Normalised;
size_line(N, Args) ->
	lists:flatten([size_line(N - 1, X) || X <- Args]).

split_line([], Args) ->
	lists:reverse(Args);
split_line([H | T], Args) ->
	Len = length(Args),
	Slice = trunc(Len/H),
	NewArgs = case Slice of
					0 -> [Args];
					_ -> split_l2(Args, Slice, ?EMPTY_ACCUMULATOR)
			  end,
	[split_line(T, X) || X <- NewArgs].

split_l2([],   _N, Acc) ->
	lists:reverse(Acc);
split_l2(List, N,  Acc) when N < length(List) ->
	split_l2([], N, [List | Acc]);
split_l2(List, N,  Acc) when N /= 0 -> % yes it ran away in an infinite loop here earlier
	{First, Rest} = lists:split(N, List),
	split_l2(Rest, N, [First | Acc]).

fmt(#'$ast¯'{do   = complex,
						 args = [R, I]}) when R < 0 andalso
																	I < 0 -> make_frag("¯~pJ¯~p", [abs(R), abs(I)]);
fmt(#'$ast¯'{do   = complex,
						 args = [R, I]}) when R < 0 -> make_frag("¯~pJ~p",  [abs(R), abs(I)]);
fmt(#'$ast¯'{do   = complex,
						 args = [R, I]}) when I < 0 -> make_frag("~pJ¯~p",  [abs(R), abs(I)]);
fmt(#'$ast¯'{do   = complex,
						 args = [R, I]})            -> make_frag("~pJ~p",   [abs(R), abs(I)]);
fmt(#'$ast¯'{} = A)                     -> [#fmt_line{segs = Strings}] = build_segments(A),
																				   {Width, Height} = get_size(Strings),
																				   #fmt_segment{strings = Strings,
																				   							width   = Width + 2,
																				   							height  = Height + 2,
																				   							boxing  = boxed};
fmt(X)												when X < 0 -> make_frag("¯~p", [abs(X)]);
fmt(X)																	 -> make_frag("~p",  [X]).

get_size(List) ->
	Padding = length(List) - 1,
	{W, H} = get_size2(List, 0, 0),
	{W + Padding, H}.

get_size2([], Width, Height) ->
	{Width, Height};
get_size2([#fmt_segment{width  = W,
						height = H} | T], Width, Height) ->
	NewH = get_greater(Height, H),
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
	% there are reasons we add extra new lines here and then take the first ones away later
	% its to make the test suites work and keep the output purty for users with multiple errors
	io_lib:format("~n~nError~n~ts~n~s~n~s (~ts:~ts) on line ~p at character ~p",
				  [E, Pointer, T, M1, M2, AtL, AtC]).
