:- module(builtins, [apply/3, gen_environ/1]).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(pretty).
:- use_module(gramma).

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

apply(builtin(/), [X], X) :- X = number_lit(_).
apply(builtin(/), [number_lit(Arg) | Args], number_lit(Value)) :-
    apply(builtin(/), Args, number_lit(Product)),
    Value is Arg / Product.

apply(builtin(=), [H | T], boolean_lit(true)) :-
    repetition(H, T).
apply(builtin(=), [H | T], boolean_lit(false)) :-
    not(repetition(H, T)).

apply(builtin(<), [number_lit(H) | T], boolean_lit(true)) :-
    forall(member(number_lit(Value), T),
	   H < Value).
apply(builtin(<), _, boolean_lit(false)).

apply(builtin(>), [number_lit(H) | T], boolean_lit(true)) :-
    forall(member(number_lit(Value), T),
	   H > Value).
apply(builtin(>), _, boolean_lit(false)).

apply(builtin(print), [X], nil_lit) :-
    pretty:print(X, Repr),
    format("~s\n", [Repr]).
apply(builtin(print), [H1, H2 | T], nil_lit) :-
    pretty:print_list([H1, H2 | T], Repr),
    format("~s\n", [Repr]).

apply(builtin(read), [string_lit(Text)], Result) :-
    gramma:parse_repl(Text, Result).

apply(builtin(input), [], Result) :-
    apply(builtin(input), [string_lit("> ")], Result).
apply(builtin(input), [string_lit(Prompt)], Result) :-
    format(Prompt),
    read_line_to_codes(user_input, String),
    Result = string_lit(String).

apply(builtin(list), Values, list_lit(Values)) :-
    not(Values = []).

apply(builtin(cons), [Head, nil_lit], list_lit([Head])).
apply(builtin(cons), [Head, Rest], list_lit([Head | Content])) :-
    Rest = list_lit(Content).

apply(builtin(car), [list_lit([Head | _])], Head).

apply(builtin(cdr), [list_lit([_])], nil_lit).
apply(builtin(cdr), [list_lit([_ | Rest])], list_lit(Rest)) :-
    not(Rest = []).

repetition(_, []).
repetition(El, [El | T]) :-
    repetition(El, T).

gen_environ(Environ) :-
    pairs_keys_values(Pairs,
		      ["+", "-", "*", "/", "=", "<", ">", "print", "read", "input", "list", "cons", "car", "cdr"],
		      [builtin(+), builtin(-), builtin(*), builtin(/), builtin(=), builtin(<), builtin(>), builtin(print), builtin(read), builtin(input), builtin(list), builtin(cons), builtin(car), builtin(cdr)]),
    list_to_assoc(Pairs, Environ).
    
