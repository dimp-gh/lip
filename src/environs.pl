:- module(environs, [env_put/4, env_get/3, env_print/1, gen_environ/1, atom_builtin/2]).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(builtins).

%% Working with environments
env_put(Environ, Name, Value, NewEnviron) :-
    put_assoc(Name, Environ, Value, NewEnviron).
env_get(Environ, Name, Value) :-
    %format("Getting value of '~s' from current environment\n", [Name]),
    get_assoc(Name, Environ, Value).
env_print(Environ) :-
    format("Environment:\n"),
    assoc_to_keys(Environ, Keys),
    forall(member(Key, Keys),
	   (
	    env_get(Environ, Key, Value),
	    format("\tkey: ~s, value: ~w\n", [Key, Value])
	   )).

atom_builtin(X, builtin(X)) :- atom(X).

gen_environ(Environ) :-
    bagof(B, builtin(B), BuiltinsAtoms),
    maplist(atom_codes, BuiltinsAtoms, Codes),
    maplist(environs:atom_builtin, BuiltinsAtoms, Builtins), 
    pairs_keys_values(Pairs, Codes, Builtins),
    list_to_assoc(Pairs, Environ).
