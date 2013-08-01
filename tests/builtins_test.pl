:- begin_tests(builtins).
:- use_module(src/builtins).

test('(+ 0)') :-
    apply(builtin(+), [number_lit(0)], number_lit(0)), !.

test('(+ 1 2 3 4)') :-
    apply(builtin(+),
	  [number_lit(1),
	   number_lit(2),
	   number_lit(3),
	   number_lit(4)
	  ],
	  number_lit(10)), !.

test('(= 1 1 1 2)') :-
    apply(builtin(=),
	  [
	   number_lit(1),
	   number_lit(1),
	   number_lit(1),
	   number_lit(2)
	  ],
	  boolean_lit(false)), !.

test('(= 0 0)') :-
    apply(builtin(=),
	  [
	   number_lit(0),
	   number_lit(0)
	  ],
	  boolean_lit(true)), !.

test('(- 5)') :-
    apply(builtin(-),
	  [number_lit(5)],
	  number_lit(-5)), !.

test('(- 10 1 2 3 4)') :-
    apply(builtin(-),
	  [
	   number_lit(10),
	   number_lit(1),
	   number_lit(2),
	   number_lit(3),
	   number_lit(4)
	  ],
	  number_lit(0)), !.

test('(* 1 2 3 4 5)') :-
    apply(builtin(*),
	  [
	   number_lit(1),
	   number_lit(2),
	   number_lit(3),
	   number_lit(4),
	   number_lit(5)
	  ],
	  number_lit(120)), !.


:- end_tests(builtins).
