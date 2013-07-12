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

:- end_tests(builtins).
