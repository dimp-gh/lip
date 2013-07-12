:- module(eval, [eval/2]).

% marking all special constructs here:
% e.g. if, define, cond, lambda,
special_term("if").

false_condition(nil_lit).
false_condition(boolean_lit(false)).

/*
 * So, here is my loose evaluation order:
 */

% literals are evaluated to themselves 
eval(number_lit(V),  number_lit(V),  _).
eval(string_lit(S),  string_lit(S),  _).
eval(boolean_lit(B), boolean_lit(B), _).
eval(nil_lit,        nil_lit,        _).

% (if (condition) then else)
% If 'Condition' is neiter false nor nil - 'Then' branch is evaluated. Otherwise 'Else' is evaluated.
eval(sexpression([id("if"), Condition, Then, _]), Result, Environ) :-
    eval(Condition, Value, Environ),
    not(false_condition(Value)),
    eval(Then, Result, Environ).
eval(sexpression([id("if"), Condition, _, Else]), Result, Environ) :-
    eval(Condition, Value, Environ),
    false_condition(Value),
    eval(Else, Result, Environ).

eval(sexpression([id(FunName) | Args]), Result, Environ) :-
    not(special_term(FunName)),
    apply(FunName, Args, Result, Environ).

eval(Expr, Result) :-
    eval(Expr, Result, initial_environ), !.
