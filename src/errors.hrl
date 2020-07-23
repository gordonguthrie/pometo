-record(error, {
                type,
                msg1,
                msg2,
                expr = "",
                at_line,
                at_char
               }).

unpostfix(Var) -> re:replace(Var, "_[0-9]+$", "").

% adding a fun in a header is a bit shite but a 1 fn utility module is worse
-compile([{nowarn_unused_function, [{unpostfix, 1}]}]).
