:- module(eval, [eval/2]).

/* So, here is my loose evaluation schema:
 */

eval(number_lit(V), number_lit(V)).
eval(string_lit(S), string_lit(S)).
eval(boolean_lit(B), boolean_lit(B)).
eval(nil_lit, nil_lit).

% (if (condition) then else)
% If 'Condition' is neiter false nor nil - 'Then' branch is evaluated. Otherwise 'Else' is evaluated.
eval(sexpression([id("if"), Condition, Then, _]), Result) :-
    eval(Condition, Value),
    not(Value = nil_lit),
    not(Value = boolean_lit(false)),
    eval(Then, Result).
eval(sexpression([id("if"), Condition, _, Else]), Result) :-
    eval(Condition, Value),
    Value = nil_lit,
    eval(Else, Result).
eval(sexpression([id("if"), Condition, _, Else]), Result) :-
    eval(Condition, Value),
    Value = boolean_lit(false),
    eval(Else, Result).

eval(sexpression([id(FunName) | Args]), Result) :-
    not(member(FunName, ["if"])),
    apply(FunName, Args, Result).
