-module(foo).
-export([server/0,client/2,start/0]).
-export([message_info/0]).

message_info() ->
    #{{'client',2} =>
	  #{'Server' => {'server',0}}}.

server() ->
    receive
	stop -> ok;
	{_A,B} when is_atom(B) -> server();
	{_A,foo} -> server();
	{_A,{_B,C}} when is_atom(C) -> server();
%	{_X,_Y} -> server();
%	2 -> server();
	F when is_atom(F), is_boolean(F) -> server();
	Y when is_pid(Y), is_atom(Y), is_boolean(Y) -> server();
	Z when is_pid(Z); is_atom(Z) -> server();
	G when is_atom(G), is_boolean(G) orelse is_boolean(G) -> server();
	Z when is_atom(Z); is_atom(Z) andalso is_pid(Z) -> server()
%	A when is_atom(A) -> server();
%	{_X} -> server()
    end.

client(_Server, 0) ->
    ok;
client(Server, N) ->
    Server ! N,
    Server ! {N, foo},
    client(Server, N-1).

start() ->    
    Server = spawn(fun server/0),
    client(Server, 10).
