:- begin_tests(gramma).
:- use_module(src/gramma).

test(parse_zero) :-
    gramma:number(number_lit(0), "0", []), !.

test(parse_number) :-
    gramma:number(number_lit(142857), "142857", []), !.

test(parse_float_number) :-
    gramma:number(number_lit(3.14), "3.14", []), !.

test(parse_empty_string) :-
    gramma:string(string_lit(""), "\"\"", []), !.

test(parse_string) :-
    gramma:string(string_lit("hello world of strings"), "\"hello world of strings\"", []), !.

test(parse_boolean_literal_true) :-
    gramma:boolean(boolean_lit(true), "#t", []), !.

test(parse_boolean_literal_false) :-
    gramma:boolean(boolean_lit(false), "#f", []), !.

test(parse_nil_literal) :-
    gramma:nil(nil_lit, "#nil", []).

test(dont_parse_empty_id) :-
    \+ gramma:identifier(id(""), "", []).

test(parse_dashed_lisp_id) :-
    gramma:identifier(id("eto-ti"), "eto-ti", []), !.

test(parse_haskell_style_id) :-
    gramma:identifier(id("<+>"), "<+>", []), !.

test(parse_trivial_sexpression) :-
    parse_repl("(quack)", sexpression([id("quack")])).

test(parse_factorial_funcion) :-
    parse_repl("(define (fact x) (if (= x 0) 1 (* x (fact (- x 1)))))",
		_).

test(skip_whitespace_while_parsing_sexprs) :-
    parse_repl("   (  whitespace  ,  whitespace     everywhere         )       ", sexpression([id("whitespace"), id(","), id("whitespace"), id("everywhere")])).

:- end_tests(gramma).
