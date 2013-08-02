:- module(eval, [eval/2]).
:- use_module(builtins).

% Marking all special constructs here,
% e.g. if, define, cond, lambda, ..
special_term("if").
special_term("lambda").

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

% Special construct IF
eval(sexpression([id("if"), Condition, Then, _]), Result, Environ) :-
    eval(Condition, Value, Environ),
    not(false_condition(Value)),
    eval(Then, Result, Environ).
eval(sexpression([id("if"), Condition, _, Else]), Result, Environ) :-
    eval(Condition, Value, Environ),
    false_condition(Value),
    eval(Else, Result, Environ).

% Special construct LAMBDA
eval(sexpression([id("lambda"), sexpression(Params), Body]), Result, _) :-
    Result = lambda(Params, Body).

% Special construct LET
eval(sexpression([id("let"), Binding, Body]), Result, Environ) :-
    Binding = sexpression([id(_), _]),
    Let = let(Binding, Body),
    eval(Let, Result, Environ).

% Special construct BLOCK
eval(sexpression([id("block") | Rest]), Result, Environ) :-
    transform_defines(Rest, Transformed),
    eval(block(Transformed), Result, Environ).

% END OF SPECIAL CONSTRUCTS

% Evaluating function calls
% Function is called by its name
eval(sexpression([id(FunName) | Args]), Result, Environ) :-
    not(special_term(FunName)),
    env_get(Environ, FunName, Function),
    apply(Function, Args, Result, Environ).

% Lambda is applied directly to arguments 
eval(sexpression([LambdaDecl | Args]), Result, Environ) :-
    %LambdaDecl = sexpression([id("lambda"), _, _]),
    eval(LambdaDecl, Lambda, Environ),
    apply(Lambda, Args, Result, Environ).

% Evaluating let
eval(let(Binding, Body), Result, Environ) :-
    Binding = sexpression([id(Name), Expr]),    
    eval(Expr, Value, Environ),
    env_put(Environ, Name, Value, NewEnviron),
    eval(Body, Result, NewEnviron).

% Evaluating block (sequence of expressions)
eval(block(Exprs), Result, Environ) :-
    eval_list_of_terms(Exprs, Results, Environ),
    last(Results, Result).

% applying lambda to arguments
apply(lambda([id(Param) | Params], Body), [Arg | Args], Result, Environ) :-
    eval(Arg, ArgValue, Environ),
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

transform_defines([], []).
% transforms '(define x 5) ...' into '(let (x 5) ...)'
transform_defines([Head | Rest], Result) :-
    Head = sexpression([id("define"), Name, Expr]),
    Name = id(_),
    LetBinding = sexpression([Name, Expr]),
    transform_defines(Rest, Transformed),
    LetBody = block(Transformed),
    Let = let(LetBinding, LetBody),
    Result = [Let].
    Result = [Let].
transform_defines([X | T1], [X | T2]) :-
    not(X = sexpression([id("define"), _, _])),
    transform_defines(T1, T2).
