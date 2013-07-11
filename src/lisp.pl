:- module(lisp, [repl]).
:- use_module(gramma).
:- use_module(eval).
/*
 * Major things to implement are:
 * - Recursive descent parser
 * - Data structure for lisp code
 * - Evaluator of cod
 */


repl :-
    nl,
    format(">>> "),
    read_line_to_codes(user_input, String),
    % format("Debug: Got string \"~s\"", [String]),
    nl,
    parse_sexpr(String, ST),
    eval(ST, Result),
    write(Result),
    repl.
