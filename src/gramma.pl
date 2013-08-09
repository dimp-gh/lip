:- module(gramma, [parse_sexpr/2, parse_repl/2, parse_module/2]).
/*
 * Basic Lisp grammar and parser. Builds some kind of syntax tree.
 * Main non-terminal: sexpr.
 * Example of usage:
 * ?- parse_sexpr("(hello world of (sexpressions) 123 \"yey\")", X).
 * 
 * X = sexpression([id([104, 101, 108, 108, 111]), id([119, 111, 114, 108, 100]), id([111, 102]), sexpression([id([115|...])]), number_lit(123), string_lit([121|...])])
 */ 

% basic building blocks
space    --> [C], { code_type(C, space) }.
digit(C) --> [C], { code_type(C, digit) }.
ascii(C) --> [C], { code_type(C, ascii) }.
alpha(C) --> [C], { code_type(C, alpha) }.
alnum(C) --> [C], { code_type(C, alnum) }.

nondelim(D) --> [D], { not(code_type(D, white)), not(memberchk(D, "()#'")) }.

% whitespace skipping stuff
optspace   --> [].
optspace   --> space, optspace.
whitespace --> space.
whitespace --> space, whitespace.


% Lisp stuff
sexpr(Content) --> "(", optspace, content(Terms), optspace, ")",
    { Content = sexpression(Terms) }.

% curly braces is a sugar for block construct
sexpr(Content) --> "{", optspace, content(Terms), optspace, "}",
    { Content = sexpression([id("block") | Terms]) }.

content([T]) --> term(T).
content([T | C]) --> term(T), whitespace, content(C).

term(L) --> literal(L).
term(S) --> sexpr(S).
term(I) --> identifier(I).
term(I) --> "'", term(T), { I = quote(T) }.

literal(N) --> number(N).
literal(S) --> string(S).
literal(B) --> boolean(B).
literal(N) --> nil(N).
literal(L) --> list(L).

% TODO: Current implementation thinks that 001 is a valid number. I don't think so. Should be fixed. 
number(D) --> decimal(D).
number(F) --> float(F).

decimal(N) --> digits(Cs), { number_codes(Value, Cs), N = number_lit(Value) }.

float(F) --> digits(Before), ".", digits(After), {
		 append(Before, ".", WithPoint),
		 append(WithPoint, After, Cs),
		 number_codes(Value, Cs),
	         F = number_lit(Value)
	     }.

digits([D]) --> digit(D).
digits([D | Ds]) --> digit(D), digits(Ds).

string(Ss) --> "\"", string_content(S), "\"", { Ss = string_lit(S) }.
string_content([]) --> [].
string_content([C | Cs]) --> ascii(C), string_content(Cs).

boolean(boolean_lit(true)) --> "#t".
boolean(boolean_lit(false)) --> "#f".

nil(nil_lit) --> "#nil".

list(L) --> "[", optspace, content(Terms), optspace, "]", { L = list_lit(Terms) }.

identifier(Id) --> nondelim(Ih), rest_of_id(Is), { Id = id([Ih | Is]) }.
rest_of_id([]) --> [].
rest_of_id([H | T]) --> nondelim(H), rest_of_id(T).

repl_input(T) --> optspace, term(T), optspace.

module(M) --> optspace, sexpr(B), optspace, { M = sexpression([id("block"), B]) }.
module(M) --> optspace, sexpr(B), module(Rest), { Rest = sexpression([id("block") | Block]), M = sexpression([id("block"), B | Block]) }.

parse_sexpr(String, Expr) :- sexpr(Expr, String, []), !.
parse_repl(String, Expr) :- repl_input(Expr, String, []), !.
parse_module(String, Expr) :- module(Expr, String, []), !.
