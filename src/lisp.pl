:- module(lisp, [repl]).
:- use_module(gramma).
:- use_module(eval).
:- use_module(pretty).


repl :-
    format(">>> "),
    read_line_to_codes(user_input, String),
    parse_sexpr(String, SyntaxTree),
    %format("Debug: parsed syntax tree ~w\n", [SyntaxTree]),
    pretty:print(SyntaxTree, PrettyST),
    format("Debug: parsed expression ~s\n", [PrettyST]),
    eval(SyntaxTree, Result),
    pretty:print(Result, PrettyResult),
    format("Answer = ~s, (ST = ~w)\n", [PrettyResult, Result]),
    repl.
