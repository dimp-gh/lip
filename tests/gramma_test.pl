:- begin_tests(gramma).
:- use_module(src/gramma).

test(parse_zero) :-
    gramma:number(number_lit(0), "0", []), !.

test(parse_number) :-
    gramma:number(number_lit(142857), "142857", []), !.

test(parse_empty_string) :-
    gramma:string_literal(string_lit(""), "\"\"", []), !.

test(parse_string_literal) :-
    gramma:string_literal(string_lit("hello world of strings"), "\"hello world of strings\"", []), !.

test(dont_parse_empty_id) :-
    \+ gramma:identifier(id(""), "", []).

test(parse_dashed_lisp_id) :-
    gramma:identifier(id("eto-ti"), "eto-ti", []), !.

test(parse_haskell_style_id) :-
    gramma:identifier(id("<+>"), "<+>", []), !.

test(parse_trivial_sexpression) :-
    parse_sexpr("(quack)", sexpression([id("quack")])).

test(parse_factorial_funcion) :-
    parse_sexpr("(define (fact x) (if (= x 0) 1 (* x (fact (- x 1)))))",
		_).

%test(skip_whitespace_while_parsing).

:- end_tests(gramma).
