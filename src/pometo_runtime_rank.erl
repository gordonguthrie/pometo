-module(pometo_runtime_rank).

-export([
					iterate_by_axis/3,
					iterate_over_rank/4,
					is_rank_valid/4,
					check_shape_compatible/5
				]).

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

-record(acc, {chunk = #{},
							final = #{}}).

-record(counts, {chunk        = ?START_COUNTING_ARGS,
								 no_of_chunks = ?START_COUNTING_ARGS,
								 input        = ?START_COUNTING_ARGS,
								 output       = ?START_COUNTING_ARGS}).

%%
%% API
%%

iterate_by_axis(#'$ast¯'{do = #'$shape¯'{dimensions = D2}} = Right,
									Rank,
									RankFns) ->
	Axes = pometo_runtime:make_axes(D2),
	Count = pometo_runtime:make_count(length(D2)),
	iterate_by_axes(Right, Axes, Count, RankFns).

iterate_over_rank(#'$ast¯'{do = #'$shape¯'{dimensions = D2}} = Right,
									Rank,
									RankFns,
									ChunkSize) ->
	Size    = pometo_runtime:get_no_of_elements_from_dims(D2),
	NewArgs = iterate(Right, Size + 1, ChunkSize, Rank, RankFns, #counts{}, #acc{}),
	Right#'$ast¯'{args = NewArgs}.

is_rank_valid(#'$ast¯'{do = ?shp(N)}, Rank, LNo, CNo) ->
	if
		Rank == 1         andalso N    == unsized_vector -> ok;
		Rank =< length(N) andalso Rank >= 1              -> ok;
		el/=se                                           ->
			pometo_runtime_errors:make_index_error_for_rank(Rank, LNo, CNo)
	end.

check_shape_compatible(Rank, LHSDim, RHSDim, LNo, CNo) ->
	Vec = lists:nth(Rank, RHSDim),
	Len = get_length(LHSDim),
	NoElems = pometo_runtime:get_no_of_elements_from_dims(LHSDim),
	case Len of
		1 -> case NoElems of
					Vec -> identical;
					1   -> scalar_LHS;
					_   -> pometo_runtime_errors:make_length_error_for_shape(NoElems, Vec, LNo, CNo)
				end;
		_ -> pometo_runtime_errors:make_domain_error_for_shape(LHSDim, LNo, CNo)
	end.

%%
%% Private Funs
%%

iterate_by_axes(Right, Axes, Count, RankFns) ->
	#'$ast¯'{do   = #'$shape¯'{},
					 args = Args} = Right,
	Enum = pometo_runtime:make_enumerable(Args),
	iterate_by_axes2(Enum, Axes, Count, RankFns, ?EMPTY_MAP).

iterate_by_axes2(_, _Axes, '$eof', _RankFns, Acc) ->
	Acc;
iterate_by_axes2(Enum, Axes, Count, RankFns, Acc) ->
	{NewEnum, Val} = pometo_runtime:get_first(Enum),
	#rank_fns{iteration_fn = IterFn} = RankFns,
	NewAcc = IterFn(Val, Count, Axes, Acc),
	NewCount = pometo_runtime:increment_count(Count, Axes),
	iterate_by_axes2(NewEnum, Axes, NewCount, RankFns, NewAcc).

iterate(_Right, N, _ChunkSize, _Rank, _RankFns, #counts{input = N}, #acc{final = Acc}) ->
	Acc;
iterate(Right, Size, ChunkSize, Rank, RankFns, #counts{chunk = ChunkSize} = Counts, Acc) ->
	#counts{no_of_chunks  = NoC,
					input         = InN,
					output        = OuN} = Counts,
	#acc{chunk = CAcc,
			 final = F} = Acc,
	#'$ast¯'{do = #'$shape¯'{dimensions = D}} = Right,
	RankLen = lists:nth(Rank, D),
	Val = pometo_runtime:get_nth(Right, InN),
	NewCAcc = maps:put(ChunkSize, Val, CAcc),
	#rank_fns{optional_LHS = Left,
						iteration_fn = IterFn,
	          inner_fn     = InnerFn} = RankFns,
	case Left of
		none ->
			{NewOuN, NewF} = IterFn(OuN, ChunkSize, NoC, RankLen, NewCAcc, InnerFn, F);
		_ ->
			{NewOuN, NewF} = IterFn(OuN, Left, ChunkSize, NoC, RankLen, NewCAcc, InnerFn, F)
	end,
	NewCounts = Counts#counts{chunk 			 = ?START_COUNTING_ARGS,
														no_of_chunks = NoC + 1,
														input 			 = InN + 1,
														output 			 = NewOuN},
	NewAcc = #acc{final = NewF},
	iterate(Right, Size, ChunkSize, Rank, RankFns, NewCounts, NewAcc);
iterate(Right, Size, ChunkSize, Rank, RankFns, Counts, Acc) ->
	#counts{chunk = CSize,
					input = InN} = Counts,
	#acc{chunk = CAcc} = Acc,
	Val = pometo_runtime:get_nth(Right, InN),
	NewCAcc = maps:put(CSize, Val, CAcc),
	NewCounts = Counts#counts{chunk = CSize + 1, input = InN + 1},
	iterate(Right, Size, ChunkSize, Rank, RankFns, NewCounts, Acc#acc{chunk = NewCAcc}).

get_length(unsized_vector)          -> 1;
get_length(List) when is_list(List) -> length(List).
