:- module(eval, [eval/2]).
:- use_module(builtins).

% Marking all special constructs here,
% e.g. if, define, cond, lambda, ..
special_term("if").

false_condition(nil_lit).
false_condition(boolean_lit(false)).

env_put(Environ, Name, Value, NewEnviron) :-
    put_assoc(Name, Environ, Value, NewEnviron).
env_get(Environ, Name, Value) :-
    get_assoc(Name, Environ, Value).


eval(Term, Result) :-
    builtins:gen_environ(Environ),
    eval(Term, Result, Environ).

/*
 * So, here is my loose evaluation order:
 */

% Literals are evaluated to themselves. 
eval(number_lit(V),  number_lit(V),  _).
eval(string_lit(S),  string_lit(S),  _).
eval(boolean_lit(B), boolean_lit(B), _).
eval(nil_lit,        nil_lit,        _).

% Identifiers are evaluated to associated
% values from current environment
eval(id(Name), Result, Environ) :-
    env_get(Environ, Name, Term),
    eval(Term, Result, Environ).

% SPECIAL CONSTRUCTS BEGIN HERE

% If 'Condition' is neiter false nor nil - 'Then'-branch is evaluated.
% Otherwise 'Else'-branch is evaluated.
eval(sexpression([id("if"), Condition, Then, _]), Result, Environ) :-
    eval(Condition, Value, Environ),
    not(false_condition(Value)),
    eval(Then, Result, Environ).
eval(sexpression([id("if"), Condition, _, Else]), Result, Environ) :-
    eval(Condition, Value, Environ),
    false_condition(Value),
    eval(Else, Result, Environ).

% END OF SPECIAL CONSTRUCTS

% evaluating function calls
eval(sexpression([id(FunName) | Args]), Result, Environ) :-
    not(special_term(FunName)),
    env_get(Environ, FunName, Function),
    apply(Function, Args, Result, Environ).

% applying lambda to arguments
apply(lambda([id(Param) | Params], Body), [Arg | Args], Result, Environ) :-
    eval(Arg, Environ, ArgValue),
    env_put(Environ, Param, ArgValue, NewEnviron),
    apply(lambda(Params, Body), Args, Result, NewEnviron).
apply(lambda([], Body), [], Result, Environ) :-
    eval(Body, Result, Environ).

% applying built-in function to arguments
apply(builtin(Name), Args, Result, Environ) :-
    eval_list_of_terms(Args, ArgVals, Environ),
    builtins:apply(builtin(Name), ArgVals, Result).

eval_list_of_terms([], [], _).
eval_list_of_terms([Arg | Args], [Val | Vals], Environ) :-
    eval(Arg, Val, Environ),
    eval_list_of_terms(Args, Vals, Environ).
