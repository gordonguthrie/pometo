-module(complex_no_lexer_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

basic_imaginary_number_lexer_test_() ->
    Str = "MyVariable ← 1 2j3 ¯3J4 5J¯6 ¯7j¯7 -2J2",
         %0123456789012345678901234567890123456789
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [{var,1,"MyVariable","MyVariable"},
           {let_op,12,[8592],[8592]},
           {int,14,"1",1},
           {maybe_complex_number,16,"2j3",{2,3}},
           {complex_number,20,"¯3J4",{-3,4}},
           {complex_number,25,"5J¯6",{5,-6}},
           {complex_number,30,"¯7j¯7",{-7,-7}},
           {ambivalent,36,"-","-"},
           {maybe_complex_number,37,"2J2",{2,2}}],
    % ?debugFmt("in basic_imaginary_number_lexer_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_imaginary_number_II_lexer_test_() ->
    Str = "J1 ← 1 2",
         %0123456789012345678901234567890123456789
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [{var,1,"J1","J1"},
           {let_op,4,[8592],[8592]},
           {int,6,"1",1},
           {int,8,"2",2}],
    % ?debugFmt("in basic_imaginary_number_II_lexer_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_imaginary_number_III_lexer_test_() ->
    Str = "J1b ← 1 2",
         %0123456789012345678901234567890123456789
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [{var,1,"J1b","J1b"},
           {let_op,5,[8592],[8592]},
           {int,7,"1",1},
           {int,9,"2",2}],
    % ?debugFmt("in basic_imaginary_number_III_lexer_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_imaginary_number_IV_lexer_test_() ->
    Str = "JjJj1b ← 1 2",
         %0123456789012345678901234567890123456789
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [{var,1,"JjJj1b","JjJj1b"},
           {let_op,8,[8592],[8592]},
           {int,10,"1",1},
           {int,12,"2",2}],
    % ?debugFmt("in basic_imaginary_number_IV_lexer_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).
