-module(pometo_runtime_monadic).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
			monadic_RUNTIME/1
		]).

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

monadic_RUNTIME(["⍴", #'$ast¯'{op = ?shp(0) = Shp} = AST]) ->
	AST#'$ast¯'{op   = Shp,
		        args = ""};
monadic_RUNTIME(["⍴", #'$ast¯'{op = ?shp(Dims) = Shp} = AST]) ->
	NewDims = length(Dims),
	NewShp = Shp#'$shape¯'{dimensions = [NewDims],
						   type       = number},
	AST#'$ast¯'{op   = NewShp,
				args = Dims};
monadic_RUNTIME(["⍳", #'$ast¯'{args    = Args,
					           line_no = LNo,
					           char_no = CNo}]) ->
	try
		Shp = #'$shape¯'{dimensions = Args,
						 type       = boolean,
						 line_no    = LNo,
						 char_no    = CNo},
		#'$ast¯'{op      = Shp,
				 args    = lists:seq(1, Args),
			     line_no = LNo,
				 char_no = CNo}
	catch _Type:_Errs ->
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