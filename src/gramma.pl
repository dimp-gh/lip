:- module(gramma, [parse_sexpr/2]).
/*
 * Basic Lisp grammar and parser. Builds some kind of syntax tree.
 * Main non-terminal: sexpr.
 * Example of usage:
 * ?- parse_sexpr("(hello world of (sexpressions) 123 \"yey\")", X).
 * 
 * X = sexpression([id([104, 101, 108, 108, 111]), id([119, 111, 114, 108, 100]), id([111, 102]), sexpression([id([115|...])]), number_lit(123), string_lit([121|...])])
 */ 

% basic building blocks
space    --> [C], { code_type(C, white) }.
digit(C) --> [C], { code_type(C, digit) }.
ascii(C) --> [C], { code_type(C, ascii) }.
alpha(C) --> [C], { code_type(C, alpha) }.
alnum(C) --> [C], { code_type(C, alnum) }.

% whitespace skipping stuff
optspace   --> [].
optspace   --> space, optspace.
whitespace --> space.
whitespace --> space, whitespace.

% Lisp stuff
sexpr(Content) --> optspace, "(", optspace, content(Terms), optspace, ")", optspace, { Content = sexpression(Terms) }.

content([T]) --> term(T).
content([T | C]) --> term(T), whitespace, content(C).

term(L) --> literal(L).
term(S) --> sexpr(S).
term(I) --> identifier(I).

literal(N) --> number(N).
literal(S) --> string_literal(S).

number(N) --> digits(Cs), { number_codes(Value, Cs), N = number_lit(Value) }.
digits([D]) --> digit(D).
digits([D | Ds]) --> digit(D), digits(Ds).

string_literal(Ss) --> [34], string_content(S), [34], { Ss = string_lit(S) }.
string_content([]) --> [].
string_content([C | Cs]) --> ascii(C), string_content(Cs).

% TODO: identifiers are now C/Pascal-flavoured, this should be refined
identifier(Id) --> alpha(Lh), rest_of_id(Ls), { Id = id([Lh | Ls]) }.
rest_of_id([]) --> [].
rest_of_id([D | Ds]) --> alnum(D), rest_of_id(Ds).

parse_sexpr(String, Expr) :- sexpr(Expr, String, []).
