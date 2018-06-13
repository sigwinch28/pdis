-module(bar).
-export([f/0]).

f() ->
    receive
	{A,_B} when is_atom(A); is_pid(A) -> ok;
	{C,D} when is_atom(C), is_pid(D) -> ok;
	{hello,E} when is_atom(E) -> ok;
	{F,G} when is_atom(F) andalso is_pid(G) -> ok
    end.
	    
