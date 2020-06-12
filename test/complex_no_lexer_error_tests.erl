-module(complex_no_lexer_error_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

basic_imaginary_number_failing_lexer_test_() ->
    Str = "_jjJjJ12ab ← 1 2 3",
         %0123456789012345678901234567890123456789
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [{maybe_varfrag,1,"_","_"},
           {j,2,"j",j},
           {j,3,"j",j},
           {var,4,"JjJ12ab","JjJ12ab"},
           {let_op,12,[8592],[8592]},
           {int,14,"1",1},
           {int,16,"2",2},
           {int,18,"3",3}],
    % ?debugFmt("in basic_imaginary_number_lexer_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).
