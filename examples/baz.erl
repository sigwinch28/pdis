-module(baz).
-export([f/0,g/2]).

f() ->
    receive
	X when is_atom(X) -> ok;
	X when is_boolean(X) -> ok;
	X when is_pid(X) -> ok;
	X when is_tuple(X) -> ok
    end.

g(Server, X) ->
    Server ! X.
	     
    
