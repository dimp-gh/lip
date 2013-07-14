:- module(builtins, [apply/3, gen_environ/1]).
:- use_module(library(assoc)).
:- use_module(library(pairs)).

% Here i'll declare all built-in-language functions, as '+', '-', 'or', 'and'

apply(builtin(+), [X], X) :- X = number_lit(_).
apply(builtin(+), [number_lit(Arg) | Args], number_lit(Value)) :-
    apply(builtin(+), Args, number_lit(Sum)),
    Value is Arg + Sum.

apply(builtin(-), [number_lit(X)], number_lit(Result)) :-
    Result is -X.
apply(builtin(-), [number_lit(Arg) | Args], number_lit(Value)) :-
    apply(builtin(+), Args, number_lit(Sum)),
    Value is Arg - Sum.

apply(builtin(*), [X], X) :- X = number_lit(_).
apply(builtin(*), [number_lit(Arg) | Args], number_lit(Value)) :-
    apply(builtin(*), Args, number_lit(Product)),
    Value is Arg * Product.

apply(builtin(=), [H | T], boolean_lit(true)) :-
    repetition(H, T).
apply(builtin(=), [H | T], boolean_lit(false)) :-
    not(repetition(H, T)).

repetition(_, []).
repetition(El, [El | T]) :-
    repetition(El, T).

gen_environ(Environ) :-
    pairs_keys_values(Pairs,
		      ["+", "-", "*", "="],
		      [builtin(+), builtin(-), builtin(*), builtin(=)]),
    list_to_assoc(Pairs, Environ).
    
