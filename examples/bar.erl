-module(bar).
-export([f/0,g/1]).

f() ->
    receive
	{A,B} when is_atom(A), is_pid(B) -> ok;
	{F, G} when is_atom(F); is_pid(G) -> ok;
	{C,D} when is_atom(C), is_pid(D) -> ok;
	{hello,_E} -> ok;
	{F,_G} when is_atom(F), is_pid(F) -> ok
    end.

g(Server) ->
    Server ! {true, 2},
    Server ! Server.
