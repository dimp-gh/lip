:- begin_tests(errors).
:- use_module(src/eval).
:- use_module(src/gramma).
:- use_module(src/pretty).

% useful routines
parse_eval_catch(String) :-
    gramma:parse_sexpr(String, SyntaxTree),
    catch(eval:eval(SyntaxTree, _), error(_), true).

% tests
test('(if not-enough-args)') :-
    parse_eval_catch("(if not-enough-args)").

test('unbound identifier') :-
    parse_eval_catch("(unbound-identifier)").

test('lambda with wrong param count') :-
    parse_eval_catch("(lambda (x) (* x x) (+ x 1))").

test('lambda with screwed arglist') :-
    parse_eval_catch("(lambda 123 (* x x))").

test('lambda with non-ids in arglist') :-
    parse_eval_catch("(lambda (x y 1)  (* x x))").

test('let with bad number of arguments') :-
    parse_eval_catch("(let 1 2 3 4)").

test('empty block') :-
    parse_eval_catch("(block)").

test('quoting multiple things') :-
    parse_eval_catch("(quote 1 2 3)").

test('eval multiple things') :-
    parse_eval_catch("(eval (quote 1) (quote 2))").

test('cond without else') :-
    parse_eval_catch("(cond (#f 0) (#nil 1) (#f 2))").

:- end_tests(errors).
