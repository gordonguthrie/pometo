-module(pometo_runtime_rank).

-export([
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

iterate_over_rank(#'$ast¯'{do   = #'$shape¯'{}}                = Left,
									#'$ast¯'{do   = #'$shape¯'{dimensions = D2}} = Right,
									 Rank,
									 IterFn) ->
	{_Discard, Keep} = lists:split(Rank, D2),
	ChunkSize = pometo_runtime:get_no_of_elements_from_dims(Keep),
	Size      = pometo_runtime:get_no_of_elements_from_dims(D2),
	NewArgs   = iterate(Left, Right, Size + 1, ChunkSize, Rank, IterFn, #counts{}, #acc{}),
	Right#'$ast¯'{args = NewArgs}.

is_rank_valid(#'$ast¯'{do = ?shp(N)}, Rank, LNo, CNo) ->
	if
		Rank == 1         andalso N    == unsized_vector -> ok;
		Rank =< length(N) andalso Rank >= 1              -> ok;
		el/=se                                           -> make_index_error(Rank, LNo, CNo)
	end.

check_shape_compatible(Rank, LHSDim, RHSDim, LNo, CNo) ->
	Vec = lists:nth(Rank, RHSDim),
	Len = get_length(LHSDim),
	NoElems = pometo_runtime:get_no_of_elements_from_dims(LHSDim),
	case Len of
		1 -> case NoElems of
					Vec -> identical;
					1   -> scalar_LHS;
					_   -> make_length_error(NoElems, Vec, LNo, CNo)
				end;
		_ -> make_domain_error(LHSDim, LNo, CNo)
	end.

%%
%% Private Funs
%%

iterate(_Left, _Right, N, _ChunkSize, _Rank, _IterFn, #counts{input = N}, #acc{final = Acc}) ->
	Acc;
iterate(Left, Right, Size, ChunkSize, Rank, IterFn, #counts{chunk = ChunkSize} = Counts, Acc) ->
	% broke out match to make pretty, but need to leave one in for function head
	#counts{no_of_chunks  = NoC,
					input         = InN,
					output        = OuN} = Counts,
	#acc{chunk = CAcc,
			 final = F} = Acc,
	#'$ast¯'{do = #'$shape¯'{dimensions = D}} = Right,
	RankLen = lists:nth(Rank, D),
	Val = pometo_runtime:get_nth(Right, InN),
	NewCAcc = maps:put(ChunkSize, Val, CAcc),
	{NewOuN, NewF} = IterFn(OuN, Left, ChunkSize, NoC, RankLen, NewCAcc, F),
	NewCounts = Counts#counts{chunk 			 = ?START_COUNTING_ARGS,
														no_of_chunks = NoC + 1,
														input 			 = InN + 1,
														output 			 = NewOuN},
	NewAcc = #acc{final = NewF},
	iterate(Left, Right, Size, ChunkSize, Rank, IterFn, NewCounts, NewAcc);
iterate(Left, Right, Size, ChunkSize, Rank, IterFn, Counts, Acc) ->
	#counts{chunk = CSize,
					input = InN} = Counts,
	#acc{chunk = CAcc} = Acc,
	Val = pometo_runtime:get_nth(Right, InN),
	NewCAcc = maps:put(CSize, Val, CAcc),
	NewCounts = Counts#counts{chunk = CSize + 1, input = InN + 1},
	iterate(Left, Right, Size, ChunkSize, Rank, IterFn, NewCounts, Acc#acc{chunk = NewCAcc}).

get_length(unsized_vector)          -> 1;
get_length(List) when is_list(List) -> length(List).

make_index_error(Rank, LNo, CNo) ->
	Msg1  = "Invalid Axis",
	Msg2  = io_lib:format("~p", [Rank]),
	Error = pometo_runtime_format:make_error("INDEX ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).

make_length_error(NoElems, Vec, LNo, CNo) ->
	Msg1  = "LHS vector doesn't match the selection axis",
	Msg2  = io_lib:format("LHS has ~p elements - RHS Axis has ~p~n", [NoElems, Vec]),
	Error = pometo_runtime_format:make_error("LENGTH ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).

make_domain_error(Len, LNo, CNo) ->
	Msg1  = "LHS must be a vector or a scalar",
	Msg2  = io_lib:format("It has a shape of ~p", [Len]),
	Error = pometo_runtime_format:make_error("DOMAIN ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).
