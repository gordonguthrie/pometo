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

monadic_RUNTIME([",", #'$ast¯'{op   = ?shp(0) = Shp,
							   args = Arg} = AST]) ->
	NewShp = Shp?shp([1]),
	AST#'$ast¯'{op   = NewShp,
				args = [Arg]};
monadic_RUNTIME([",", #'$ast¯'{op   = ?shp(_N) = Shp,
							   args = Args} = AST]) ->
	NewShp = Shp?shp([length(Args)]),
	AST#'$ast¯'{op = NewShp};
monadic_RUNTIME(["⍴", #'$ast¯'{op = ?shp(0) = Shp} = AST]) ->
	AST#'$ast¯'{op   = Shp,
		        args = ""};
% need to know the size for ⍴ so size it and flip it back in
monadic_RUNTIME(["⍴", #'$ast¯'{op   = ?shp(unsized_vector) = Shp,
							   args = Args} = AST]) ->
	Dims = [length(Args)],
	monadic_RUNTIME(["⍴", AST#'$ast¯'{op = Shp?shp(Dims)}]);
monadic_RUNTIME(["⍴", #'$ast¯'{op = ?shp(Dims) = Shp} = AST]) ->
	NewDims = length(Dims),
	NewShp = Shp#'$shape¯'{dimensions = [NewDims],
						   type       = number},
	AST#'$ast¯'{op   = NewShp,
				args = Dims};
monadic_RUNTIME(["⍳", #'$ast¯'{op      = ?shp(D),
							   args    = Args,
					           line_no = LNo,
					           char_no = CNo}]) ->
	NewArgs = case D of
					0 -> [Args];
					_ -> Args
			   end,
	case pometo_runtime:are_all_positive_integers(NewArgs) of
		true ->
			Shp = #'$shape¯'{dimensions = NewArgs,
							 type       = array,
							 line_no    = LNo,
							 char_no    = CNo},
			NewArgs2 = make_args(lists:reverse(NewArgs), LNo, CNo),
			#'$ast¯'{op      = Shp,
					 args    = NewArgs2,
				     line_no = LNo,
					 char_no = CNo};
		false ->
			Msg1 = "⍳ only accepts integer arguments and was called with",
			Msg2 = io_lib:format("~p", [Args]),
			Error = pometo_runtime_format:make_error("DOMAIN ERROR", Msg1, Msg2, LNo, CNo),
			throw({error, Error})
	end;
monadic_RUNTIME([Op, #'$ast¯'{op   = ?shp(0),
	                          args = A} = L]) ->
	NewA = execute_monadic(Op, A),
	L#'$ast¯'{args = NewA};
monadic_RUNTIME([Op, #'$ast¯'{op   = ?shp(_N),
	                          args = A} = L]) ->
	NewA = [execute_monadic(Op, X) || X <- A],
	L#'$ast¯'{args = NewA}.

%%
%% Helper functions
%%
make_args(Args, LNo, CNo) ->

	NewArgs = make_args2(Args),
	%% the indices can either be scalars or vectors depending
	%% on how many arguments are passed to ⍴
	case length(Args) of
		1 ->
			[X || [X] <- NewArgs];
		_ ->
			Shp = #'$shape¯'{dimensions = 0,
							 type       = array,
							 line_no    = LNo,
							 char_no    = CNo},
			AST = #'$ast¯'{op = Shp,
						   line_no = LNo,
						   char_no = CNo},
			[AST#'$ast¯'{args = X} || X <- NewArgs]
	end.

make_args2([]) -> [];
make_args2([H | T]) ->
	Seq = lists:seq(1, H),
	SecondSeq = make_args2(T),
	case SecondSeq of
		[] -> [[X]     || X <- Seq];
		_  -> [[X | Y] || X <- Seq,
	                       Y <- SecondSeq]
	end.

%% complex nos first
execute_monadic("+", #'$ast¯'{op   = complex,
                              args = [R, I]} = A) -> A#'$ast¯'{args = [ R, -I]};
execute_monadic("-", #'$ast¯'{op   = complex,
                              args = [R, I]} = A) -> A#'$ast¯'{args = [-R, -I]};
execute_monadic("×", #'$ast¯'{op   = complex,
                              args = [R, I]} = A) -> Mag = math:sqrt(R * R + I * I),
												     A#'$ast¯'{args = [R/Mag,  I/Mag]};
execute_monadic("÷", #'$ast¯'{op   = complex,
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