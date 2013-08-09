:- module(eval, [eval/2, eval_safe/2]).
:- use_module(builtins).

% Marking all special constructs here,
% e.g. if, define, cond, lambda, ..
special_term("if").
special_term("lambda").
special_term("let").
special_term("block").
special_term("quote").
special_term("eval").
special_term("else").

false_condition(nil_lit).
false_condition(boolean_lit(false)).

env_put(Environ, Name, Value, NewEnviron) :-
    put_assoc(Name, Environ, Value, NewEnviron).
env_get(Environ, Name, Value) :-
    get_assoc(Name, Environ, Value).


eval(Term, Result) :-
    builtins:gen_environ(Environ),
    eval(Term, Result, Environ), !.

/*
 * So, here is my loose evaluation order:
 */

% Literals are evaluated to themselves. 
eval(number_lit(V),  number_lit(V),  _).
eval(string_lit(S),  string_lit(S),  _).
eval(boolean_lit(B), boolean_lit(B), _).
eval(nil_lit,        nil_lit,        _).
eval(list_lit(X),    list_lit(X),    _).

% Identifiers are evaluated to associated
% values from current environment
eval(id(Name), Result, Environ) :-
    not(special_term(Name)), % TODO: not builtin check
    env_get(Environ, Name, Term),
    eval(Term, Result, Environ).

% Case when identifier isn't in current scope
eval(id(Name), _, Environ) :-
    not(special_term(Name)),
    not(env_get(Environ, Name, _)),
    format_to_chars("Unknown identifier ~s", [Name], ErrorMsg),
    throw(error(ErrorMsg)).

% SPECIAL CONSTRUCTS BEGIN HERE

% TODO: move special constructors (if, cond, lambda, block, let) to desugaring part

% Special construct IF
eval(sexpression([id("if"), Condition, Then, _]), Result, Environ) :-
    eval(Condition, Value, Environ),
    not(false_condition(Value)),
    eval(Then, Result, Environ).
eval(sexpression([id("if"), Condition, _, Else]), Result, Environ) :-
    eval(Condition, Value, Environ),
    false_condition(Value),
    eval(Else, Result, Environ).
% Case when `if` doesn't get its three required arguments
eval(sexpression([id("if") | Args]), _, _) :-
    length(Args, ArgCount),
    ArgCount \= 3,
    throw(error("'if' must have exactly three arguments: condition, true-branch and false-branch")).

% Special construct COND
eval(sexpression([id("cond") | Branches]), Result, Environ) :-
    select_branch(Branches, Expr, Environ),
    eval(Expr, Result, Environ).

% Special construct LAMBDA
eval(sexpression([id("lambda"), sexpression(Params), Body]), Result, _) :-
    Result = lambda(Params, Body).
% Case when lambda gets bad argument list
eval(sexpression([id("lambda"), Params, _]), _, _) :-
    not(Params = sexpression(_)),
    throw(error("'lambda' argument list should be list")).
% Case when lambda argument list contains non-identifiers
eval(sexpression([id("lambda"), sexpression(Params), _]), _, _) :-
    not(forall(member(Param, Params), Param = id(_))),
    throw(error("'lambda' argument list should contain only identifiers")).
% Case when `lambda` doesn't get its two arguments
eval(sexpression([id("lambda") | Args]), _, _) :-
    length(Args, ArgCount),
    ArgCount \= 2,
    throw(error("'lambda' must have exactly two arguments: argument list and body")).

% Special construct LET
eval(sexpression([id("let"), Binding, Body]), Result, Environ) :-
    Binding = sexpression([id(_), _]),
    Let = let(Binding, Body),
    eval(Let, Result, Environ).
% Case when `let` doesn't get its arguments
eval(sexpression([id("let") | Args]), _, _) :-
    length(Args, ArgCount),
    ArgCount \= 2,
    throw(error("'let' must have exactly two arguments: argument list and body")).

% Special construct BLOCK
eval(sexpression([id("block") | Rest]), Result, Environ) :-
    transform_defines(Rest, Transformed),
    eval(block(Transformed), Result, Environ).
eval(sexpression([id("block")]), _, _) :-
    throw(error("cannot evaluate empty block")).

% Special construct QUOTE
eval(sexpression([id("quote"), Thing]), Result, _) :-
    Result = quote(Thing).
% Case when `quote` doesn't get one argument
eval(sexpression([id("quote") | Rest]), _, _) :-
    length(Rest, ArgCount),
    ArgCount \= 1,
    format_to_chars("'quote' must have exactly one thing to quote", [], ErrorMsg),
    throw(error(ErrorMsg)).

% Special function eval
% Eval is separated into core function because it should have
% certain behaviour when evaling quotations
eval(sexpression([id("eval"), Thing]), Result, Environ) :-
    eval(Thing, Evald, Environ),
    Evald = sexpression(_),
    eval(Evald, Result, Environ).
eval(sexpression([id("eval"), Thing]), Result, Environ) :-
    eval(Thing, Evald, Environ),
    Evald = quote(Something),
    eval(Something, Result, Environ).
eval(sexpression([id("eval"), Thing]), Result, Environ) :-
    eval(Thing, Evald, Environ),
    Result = Evald.
eval(sexpression([id("eval"), quote(Thing)]), Result, Environ) :-
    eval(Thing, Result, Environ).

% Case when `eval` gets wrong number of args
eval(sexpression([id("eval") | Rest]), _, _) :-
    length(Rest, ArgCount),
    ArgCount \= 1,
    format_to_chars("'eval' must have exactly one argument", [], ErrorMsg),
    throw(error(ErrorMsg)).

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
% transform '(define (inc x) (+ 1 x)) ...'
% into      '(let (inc (lambda (x) (+ 1 x))) ...)'
transform_defines([Head | Rest], Result) :-
    Head = sexpression([id("define"), FunHeader, FunBody]),
    FunHeader = sexpression([id(FunName) | Args]),
    FunBody = sexpression(_Expr),
    Lambda = sexpression([id("lambda"), sexpression(Args), FunBody]),
    LetBinding = sexpression([id(FunName), Lambda]),
    transform_defines(Rest, Transformed),
    LetBody = block(Transformed),
    Let = let(LetBinding, LetBody),
    Result = [Let].
transform_defines([X | T1], [X | T2]) :-
    not(X = sexpression([id("define"), _, _])),
    transform_defines(T1, T2).

select_branch([], _, _) :-
    throw(error("No condition evaluated to #t and no else branch")).
select_branch([sexpression([id("else"), Winner])], Winner, _).
select_branch([sexpression([Condition, Expr]) | Rest], Winner, Environ) :-
    eval(Condition, Result, Environ),
    false_condition(Result) ->
	select_branch(Rest, Winner, Environ) ; Winner = Expr.

eval_safe(Term, Result) :-
    catch(eval(Term, Result),
	  error(Message),
	  (Result = nil_lit, format("Evaluation error: ~s\n", [Message]))).
      % TODO: Fix ^ that. Error should not evaluate to nil. 
