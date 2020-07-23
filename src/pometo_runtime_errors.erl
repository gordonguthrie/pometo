-module(pometo_runtime_errors).

-export([
				 make_error/5,
				 make_index_error_for_rank/3,
				 make_length_error_for_reduce/4,
				 make_length_error_for_shape/4,
				 make_domain_error_for_shape/3
				]).

-include("errors.hrl").

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

make_index_error_for_rank(Rank, LNo, CNo) ->
	Msg1  = "Invalid Axis",
	Msg2  = io_lib:format("~p", [Rank]),
	Error = make_error("INDEX ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).

make_length_error_for_reduce(WindowSize, ChunkSize, LNo, CNo) ->
	NewSize = if
			is_list(WindowSize) -> WindowSize;
			is_map(WindowSize)  -> {_Disc, Keep} = lists:unzip(lists:sort(maps:to_list(WindowSize))),
														 Keep
	end,
	Msg1  = "Reduction window is too long for the axis",
	Msg2  = io_lib:format("LHS has window size of ~p elements - RHS Axis has ~p~n", [NewSize, ChunkSize]),
	Error = make_error("LENGTH ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).

make_length_error_for_shape(NoElems, Vec, LNo, CNo) ->
	Msg1  = "LHS vector doesn't match the selection axis",
	Msg2  = io_lib:format("LHS has ~p elements - RHS Axis has ~p~n", [NoElems, Vec]),
	Error = make_error("LENGTH ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).

make_domain_error_for_shape(Len, LNo, CNo) ->
	Msg1  = "LHS must be a vector or a scalar",
	Msg2  = io_lib:format("It has a shape of ~p", [Len]),
	Error = make_error("DOMAIN ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).
