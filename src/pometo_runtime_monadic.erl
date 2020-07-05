-module(pometo_runtime_monadic).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
					monadic_RUNTIME/1
				]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

monadic_RUNTIME([[","], #'$ast¯'{do   = ?shp(0) = Shp,
																 args = Arg} = AST]) ->

	NewShp = Shp?shp([1]),
	AST#'$ast¯'{do   = NewShp,
							args = [Arg]};
monadic_RUNTIME([[","], #'$ast¯'{do   = ?shp(_N) = Shp,
																 args = Args} = AST]) ->
	NewShp = Shp?shp([length(Args)]),
	AST#'$ast¯'{do = NewShp};
monadic_RUNTIME([["⍴"], #'$ast¯'{do = ?shp(0) = Shp} = AST]) ->
	AST#'$ast¯'{do   = Shp,
							args = ""};
% need to know the size for ⍴ so size it and flip it back in
monadic_RUNTIME([["⍴"], #'$ast¯'{do   = ?shp(unsized_vector) = Shp,
																 args = Args} = AST]) ->
	Dims = [length(Args)],
	monadic_RUNTIME([["⍴"], AST#'$ast¯'{do = Shp?shp(Dims)}]);
monadic_RUNTIME([["⍴"], #'$ast¯'{do = ?shp(Dims) = Shp} = AST]) ->
	NewDims = length(Dims),
	NewShp = Shp#'$shape¯'{dimensions = [NewDims],
												 indexed    = false,
												 type       = number},
	AST#'$ast¯'{do   = NewShp,
							args = Dims};
monadic_RUNTIME([["⍳"], #'$ast¯'{do      = ?shp(D),
																 args    = Args,
																 line_no = LNo,
																 char_no = CNo}]) ->
	NewArgs = case D of
					0 -> [Args];
					_ -> Args
				 end,
	case pometo_runtime:are_all_positive_integers(NewArgs) of
		true ->
			NewDs = pometo_runtime:make_dimensions(NewArgs),
			Shp = #'$shape¯'{dimensions = NewDs,
											 indexed    = false,
											 type       = array,
											 line_no    = LNo,
											 char_no    = CNo},
			IsScalar = case D of
				unsized_vector -> false;
				0              -> true;
				_              -> false
			end,
			NewArgs2 = make_args(pometo_runtime:args_reverse(NewArgs), IsScalar, LNo, CNo),
			#'$ast¯'{do      = Shp,
							 args    = NewArgs2,
							 line_no = LNo,
							 char_no = CNo};
		false ->
			Msg1 = "⍳ only accepts integer arguments and was called with",
			Msg2 = io_lib:format("~p", [pometo_runtime:args_to_list(Args)]),
			Error = pometo_runtime_format:make_error("DOMAIN ERROR", Msg1, Msg2, LNo, CNo),
			throw({error, Error})
	end;
monadic_RUNTIME([Do, #'$ast¯'{do   = ?shp(0),
															args = A} = L]) ->
	NewA = do_apply(Do, A, fun execute_monadic/2),
	L#'$ast¯'{args = NewA};
monadic_RUNTIME([Do, #'$ast¯'{do   = #'$shape¯'{},
															args = A} = L]) ->
	ApplyFun = fun(X) ->
		do_apply(Do, X, fun execute_monadic/2)
	end,
	NewA = apply_to_args(ApplyFun, A),
	L#'$ast¯'{args = NewA};
monadic_RUNTIME(List) ->
	[io:format("in monadic_RUNTIME wigging out with ~p~n", [X]) || X <- List],
	bergamotto.

%%
%% Helper functions
%%

do_apply([],      Val, _Fn) -> Val;
do_apply([H | T], Val, Fn)  -> A = Fn(H, Val),
															 do_apply(T, A, Fn).

apply_to_args(Fn, Args) when is_list(Args) -> [Fn(X) || X <- Args];
apply_to_args(Fn, Args) when is_map(Args)  -> I = maps:iterator(Args),
																							apply_to_map_Val(Fn, Args, I).

apply_to_map_Val(_Fn, Map, none) when is_map(Map) -> Map;
apply_to_map_Val(Fn,  Map, I)    when is_map(Map) -> {K, V, NewI} = maps:next(I),
																										 NewMap = maps:put(K, Fn(V), Map),
																										 apply_to_map_Val(Fn, NewMap, NewI).

make_args(Args, IsScalar, LNo, CNo) ->
	NewArgs = make_args2(Args),
	%% the indices can either be scalars or vectors depending
	%% on how many arguments are passed to ⍴
	case IsScalar of
		true  -> [X || [X] <- NewArgs];
		false -> Shp = #'$shape¯'{dimensions = 0,
															type       = array,
															line_no    = LNo,
															char_no    = CNo},
						 AST = #'$ast¯'{do      = Shp,
														line_no = LNo,
														char_no = CNo},
						 [AST#'$ast¯'{args = X} || X <- NewArgs]
	end.

make_args2([]) -> [];
%make_args2(M) when is_map(M) -> Iterator = maps:iterator(M),
%																make_args3(Iterator);
make_args2([H | T]) 				 -> Seq = lists:seq(1, H),
																SecondSeq = make_args2(T),
																case SecondSeq of
																	[] -> [[X]     || X <- Seq];
																	_  -> [[X | Y] || X <- Seq,
																										Y <- SecondSeq]
																end.

%make_args3(none)     -> [];
%make_args3(Iterator) -> {_K, V, NewI} = maps:next(Iterator),
%												Seq = lists:seq(1, V),
%												SecondSeq = make_args3(NewI),
%												case SecondSeq of
%													[] -> [[X]     || X <- Seq];
%													_  -> [[X | Y] || X <- Seq,
%																						Y <- SecondSeq]
%												end.

%% complex nos first
execute_monadic("+", #'$ast¯'{do   = complex,
															args = [R, I]} = A) -> A#'$ast¯'{args = [ R, -I]};
execute_monadic("-", #'$ast¯'{do   = complex,
															args = [R, I]} = A) -> A#'$ast¯'{args = [-R, -I]};
execute_monadic("×", #'$ast¯'{do   = complex,
															args = [R, I]} = A) -> Mag = math:sqrt(R * R + I * I),
															A#'$ast¯'{args = [R/Mag,  I/Mag]};
execute_monadic("÷", #'$ast¯'{do   = complex,
															args = [R, I]} = A) -> Sq = R * R + I * I,
															A#'$ast¯'{args = [R/Sq, -I/Sq]};

%% then plain ones
execute_monadic("+", V) -> V; % complex conjugate stub. return identity.
execute_monadic("-", V) -> -1 * V;
execute_monadic("×", V) -> signum(V); % when complex numbers are introduced, this becomes {⍵÷|⍵}.
execute_monadic("÷", V) -> 1 / V;
execute_monadic("|", V) -> abs(V).

signum(V) when V <  0 -> -1;
signum(V) when V == 0 -> 0;
signum(V) when V >  0 -> 1.