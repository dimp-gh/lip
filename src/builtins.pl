:- module(builtins, [apply/3, gen_environ/1]).
:- use_module(library(assoc)).
:- use_module(library(pairs)).

% Here i'll declare all built-in-language functions, as '+', '-', 'or', 'and'

apply(builtin(+), [X], X).
apply(builtin(+), [number_lit(Arg) | Args], number_lit(Value)) :-
    apply(builtin(+), Args, number_lit(Sum)),
    Value is Arg + Sum.

gen_environ(Environ) :-
    pairs_keys_values(Pairs, ["+"], [builtin(+)]),
    list_to_assoc(Pairs, Environ).
    
