:- module(desugar, [transform/2, transform_safe/2]).
%% Transforming all special constructors from
%% simply parsed syntax tree to their syntax tree representations.
%% For example, `sexpression("if", Condition, Then, Else)` gets transformed to
%% `if(Condition, Then, Else)`.


%% safe wrapper for `transform`
transform_safe(ParsedTree, DesugaredTree) :-
    catch(transform(ParsedTree, DesugaredTree),
	  error(Message),
	  (
	   format("Transformation error: ~s\n", [Message]),
	   fail
	  )).

%% Signature:
%% tranform(ParsedSyntaxTree, DesugaredTree).

% `if` constructor
transform(sexpression([id("if"), Condition, Then, Else]),
	  if(TransCondition, TransThen, TransElse)) :-
    transform(Condition, TransCondition),
    transform(Then, TransThen),
    transform(Else, TransElse).
% Case when `if` doesn't get its three required arguments
transform(sexpression([id("if") | Args]), _) :-
    length(Args, ArgCount),
    ArgCount \= 3,
    throw(error("'if' must have exactly three arguments: condition, true-branch and false-branch")).

% `cond` constructor
transform(sexpression([id("cond") | Branches]),
	  cond(TransBranches)) :-
    map_transform(Branches, TransBranches).

% `lambda` constructor
transform(sexpression([id("lambda"), sexpression(Params), Body]),
	  lambda(Params, TransBody)) :-
    format("transform(lambda)\n"),
    transform(Body, TransBody).
% Case when lambda gets bad argument list
transform(sexpression([id("lambda"), Params, _]), _) :-
    not(Params = sexpression(_)),
    throw(error("'lambda' argument list should be list")).
% Case when lambda argument list contains non-identifiers
transform(sexpression([id("lambda"), sexpression(Params), _]), _) :-
    not(forall(member(Param, Params), Param = id(_))),
    throw(error("'lambda' argument list should contain only identifiers")).
% Case when lambda doesn't get its two arguments
transform(sexpression([id("lambda") | Args]), _) :-
    length(Args, ArgCount),
    ArgCount \= 2,
    throw(error("'lambda' must have exactly two arguments: argument list and body")).

% `let` constructor
transform(sexpression([id("let"), Binding, Body]),
	  let(Name, TransExpr, TransBody)) :-
    Binding = sexpression([id(Name), Expr]),
    transform(Expr, TransExpr),
    transform(Body, TransBody).
% Case when `let` doesn't get its arguments
transform(sexpression([id("let") | Args]), _) :-
    length(Args, ArgCount),
    ArgCount \= 2,
    throw(error("'let' must have exactly two arguments: name-value binding and body")).

% `block` constructor
transform(sexpression([id("block") | Rest]), Result) :-
    transform_defines(Rest, Transformed),
    %map_transform(Transformed, FullyTransformed),
    Result = block(Transformed).
transform(sexpression([id("block")]), _) :-
    throw(error("cannot transform empty block")).

% `quote` constructor
transform(sexpression([id("quote"), Thing]), quote(TransThing)) :-
    transform(Thing, TransThing).
% Case when `quote` doesn't get one argument
transform(sexpression([id("quote") | Rest]), _) :-
    length(Rest, ArgCount),
    ArgCount \= 1,
    format_to_chars("'quote' must have exactly one thing to quote", [], ErrorMsg),
    throw(error(ErrorMsg)).

% Anything else is not transformed
transform(X, X).

map_transform([Expr], [TransExpr]) :-
    transform(Expr, TransExpr).
map_transform([Expr | Rest], [TransExpr, TransRest]) :-
    transform(Expr, TransExpr),
    map_transform(Rest, TransRest).

% transform '(define (inc x) (+ 1 x)) ...'
% into      '(let (inc (lambda (x) (+ 1 x))) ...)'
% Takes a list of sexpressions and returns a list of transformed sexpressions
transform_defines([Head | Rest], Result) :-
    format("transforming defines: started on ~w\n", [Head]),
    Head = sexpression([id("define"), FunHeader, FunBody]),
    FunHeader = sexpression([id(FunName) | Args]),
    FunBody = sexpression(_Expr),
    format("transforming defines: parameters matched\n"),
    Lambda = sexpression([id("lambda"), sexpression(Args), FunBody]),
    transform(Lambda, TransformedLambda),
    format("transforming defines: lambda transformed into ~w\n", [TransformedLambda]),
    format("transforming defines: transforming the rest\n"),
    transform_defines(Rest, TransformedRest),
    format("transforming defines: rest transformed, forming let\n"),
    LetBinding = sexpression([id(FunName), TransformedLambda]),
    LetBody = sexpression([id("block") | TransformedRest]),
    Let = let(LetBinding, TransformedLetBody),
    Result = [Let],
    format("transforming defines: succeeded\n").
transform_defines([X], [X]) :-
    format("transforming defines: skipping ~w transformation\n", [X]).
transform_defines([X | T1], [X | T2]) :-
    format("transforming defines: skipping ~w transformation\n", [X]),
    transform_defines(T1, T2).
