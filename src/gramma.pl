optspace --> whitespace.
optspace --> [].

whitespace --> " ".
whitespace --> " ", whitespace.

sexpr(Content) --> optspace, "(", optspace, content(Content), optspace, ")", optspace.

content([T]) --> term(T).
content([T | C]) --> term(T), whitespace, content(C).

term(L) --> literal(L).
term(I) --> identifier(I).
%term(I) --> quotation(Q).
term(S) --> sexpr(S).


literal(N) --> number(N).
%literal(S) --> string_literal(S).

number(C) --> digits(C).
digits([D | Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(C) --> [C], { code_type(C, digit) }.

alpha(L) --> [L], { code_type(L, alpha) }.
alnum(L) --> [L], { code_type(L, alnum) }.

identifier([Lh | Ls]) --> alpha(Lh), rest_of_id(Ls).

rest_of_id([D | Ds]) --> alnum(D), rest_of_id(Ds).
rest_of_id([]) --> [].
