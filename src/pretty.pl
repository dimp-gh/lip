:- module(pretty, [print/2]).
/*
 * Prettyprinter for Lisp AST
 */ 

print(sexpression(Terms), Result) :-
    print_list(Terms, Content),
    format_to_chars("(~s)", [Content], Result).

print(number_lit(Value), Result) :-
    number_codes(Value, Result).

print(string_lit(Value), Result) :-
    format_to_chars("\"~s\"", [Value], Result).

print(boolean_lit(true), "#t").
print(boolean_lit(false), "#f").
print(nil_lit, "#nil").

print(id(Name), Name).

print(lambda(Args, Body), Result) :-
   print(sexpression(Args), PArgs),
   print(Body, PBody),
   format_to_chars("(lambda ~s ~s)", [PArgs, PBody], Result).

print(let(Binding, Expr), Result) :-
    print(Binding, PBinding),
    print(Expr, PExpr),
    format_to_chars("(let ~s ~s)", [PBinding, PExpr], Result).

print(block(Exprs), Result) :-
    print_list(Exprs, PExprs),
    format_to_chars("(block ~s)", [PExprs], Result).

print(quote(Thing), Result) :-
    print(Thing, PThing),
    format_to_chars("(quote ~s)", [PThing], Result).

print_list([T], Result) :- print(T, Result).
print_list([H | T], Result) :-
    print_list(T, Rest),
    print(H, Head),
    format_to_chars("~s ~s", [Head, Rest], Result).
