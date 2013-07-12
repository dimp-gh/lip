:- module(lisp, [repl]).
:- use_module(gramma).
:- use_module(eval).
/*
 * Major things to implement are:
 * - Recursive descent parser
 * - Data structure for lisp code
 * - Evaluator of cod
 */

prettify(number_lit(Value), Pretty) :-
    number_codes(Value, Pretty).
prettify(string_lit(X), X).
prettyfy(boolean_lit(true), "#t").
prettyfy(boolean_lit(false), "#f").
prettyfy(nil_lit, "#nil").

repl :-
    nl,
    format(">>> "),
    read_line_to_codes(user_input, String),
    % format("Debug: Got string \"~s\"", [String]),
    nl,
    parse_sexpr(String, ST),
    eval(ST, Result),
    prettify(Result, Pretty),
    format("Answer = ~s, (AST = ~w)", [Pretty, Result]),
    repl.
