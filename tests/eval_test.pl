:- begin_tests(eval).
:- use_module(src/eval).
:- use_module(src/gramma).
:- use_module(src/pretty).

% useful routines
parse_eval(String, Result) :-
    gramma:parse_repl(String, SyntaxTree),
    eval:eval_safe(SyntaxTree, Result).

parse_eval_pretty(String, Answer) :-
    parse_eval(String, Result),
    pretty:print(Result, Answer), !.

% tests
test('function application by function name') :-
    parse_eval_pretty("(+ 1 2 3 4 5)", "15").

test('#t is a true condition') :-
    parse_eval_pretty("(if #t 1 0)", "1").

test('#f is a false condition') :-
    parse_eval_pretty("(if #f 1 0)", "0").

test('#nil is a false condition') :-
    parse_eval_pretty("(if #nil 1 0)", "0").

test('applying lambda directly to its args') :-
    parse_eval_pretty("((lambda (x y z) (+ x y z)) 1 2 3)", "6").

test('applying lambda by condition') :-
    parse_eval_pretty("((if #t (lambda (x) (+ x 1)) (lambda (x) (+ x 2))) 3)", "4").

test('using let to create a recursive function') :-
    X = "(let (fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1)))))) (fact 5))",
    parse_eval_pretty(X, "120").

test('evaluated block returns result of evaluating last expression') :-
    X = "(block 1 2 3 4 5)",
    parse_eval_pretty(X, "5").

test('transforming defines into lets') :-
    X = "(block (define x 42) 1 2 x)",
    parse_eval_pretty(X, "42").

test('more complex defines transforming') :-
    X = "(block (define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1)))))) (fact 5))",
    parse_eval_pretty(X, "120").

test('define-function transforming sugar') :-
    X = "(block (define (inc x) (+ x 1)) (inc 5))",
    parse_eval_pretty(X, "6").

test('quotations') :-
    X = "(quote (+ 1 2))",
    parse_eval_pretty(X, X).

test('evaling quotations') :-
    X = "(eval (quote (+ 1 2 3)))",
    parse_eval_pretty(X, "6").

test('list constructor') :-
    X = "(list 1 2 3)",
    parse_eval_pretty(X, "(list 1 2 3)").

test('cons cells') :-
    X = "(cons 1 (cons 2 (cons 3 #nil)))",
    parse_eval_pretty(X, "(list 1 2 3)").

test('car list') :-
    X = "(car (list 1 2 3 4))",
    parse_eval_pretty(X, "1").

test('cdr list') :-
    X = "(cdr (list 1 2 3 4))",
    parse_eval_pretty(X, "(list 2 3 4)").

test('eval read') :-
    X = "(eval (read \"(+ 1 2)\"))",
    parse_eval_pretty(X, "3").

test('simple cond') :-
    X = "(cond (#f 0) (#t 1))",
    parse_eval_pretty(X, "1").

test('cond else') :-
    X = "(cond (#f 0) (#nil 1) (else 2))",
    parse_eval_pretty(X, "2").

test('complex cond') :-
    X = "(cond ((> 5 0) 5) ((< 5 0) (- 0 5)) ((= 5 0) 0))",
    parse_eval_pretty(X, "5").

:- end_tests(eval).
