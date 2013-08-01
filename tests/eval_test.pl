:- begin_tests(eval).
:- use_module(src/eval).
:- use_module(src/gramma).
:- use_module(src/pretty).

% useful routines
parse_eval(String, Result) :-
    gramma:parse_sexpr(String, SyntaxTree),
    eval:eval(SyntaxTree, Result).

parse_eval_pretty(String, Answer) :-
    parse_eval(String, Result),
    pretty:print(Result, Answer).

% tests
test('function application by function name') :-
    parse_eval_pretty("(+ 1 2 3 4 5)", "15"), !.

test('#t is a true condition') :-
    parse_eval_pretty("(if #t 1 0)", "1"), !.

test('#f is a false condition') :-
    parse_eval_pretty("(if #f 1 0)", "0"), !.

test('#nil is a false condition') :-
    parse_eval_pretty("(if #nil 1 0)", "0"), !.

test('applying lambda directly to its args') :-
    parse_eval_pretty("((lambda (x y z) (+ x y z)) 1 2 3)", "6"), !.

test('applying lambda by condition') :-
    parse_eval_pretty("((if #t (lambda (x) (+ x 1)) (lambda (x) (+ x 2))) 3)", "4"), !.

test('using let to create a recursive function') :-
    X = "(let (fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1)))))) (fact 5))",
    parse_eval_pretty(X, "120"), !.

test('evaluated block returns result of evaluating last expression') :-
    X = "(block 1 2 3 4 5)",
    parse_eval_pretty(X, "5"), !.

test('transforming defines into lets') :-
    X = "(block (define x 42) 1 2 x)",
    parse_eval_pretty(X, "42"), !.

test('more complex defines transforming') :-
    X = "(block (define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1)))))) (fact 5))",
    parse_eval_pretty(X, "120"), !.

:- end_tests(eval).
