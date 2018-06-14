-module(pdis_logger).
-behaviour(gen_server).

-export([start_link/0]).
-export([set_module/2,add_warning/2,add_error/2,add_info/2,print/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_module(Name, File) ->
    gen_server:call(?MODULE, {set_module, Name, File}).

add_warning(Warning, Line) ->
    gen_server:call(?MODULE, {warning, Warning, Line}).

add_error(Error, Line) ->
    gen_server:call(?MODULE, {error, Error, Line}).

add_info(Ok, Line) ->
    gen_server:call(?MODULE, {ok, Ok, Line}).

print() ->
    gen_server:call(?MODULE, print).

init(_Args) ->
    {ok, {[], undefined, undefined}}.

handle_call({set_module, _Name, File}, _From, State) ->
    case file:open(File, [read]) of
	{ok, Device} ->
	    Contents = get_lines(Device),
	    {reply, ok, {[], Contents, File}};
	Err ->
	    {reply, {error, Err}, State}
    end;
handle_call(print, _From, {Acc, Contents, File}) ->
    Msgs = lists:keysort(3, Acc),
    lists:foreach(fun(Msg) ->
			  print_msg(Msg, Contents, File)
		  end, Msgs),
    {reply, ok, {Acc, Contents, File}};
handle_call({warning, Warning, Line}, _From, {Acc, Contents, File}) ->
    {reply, ok, {[{warning, Warning, Line}|Acc], Contents, File}};
handle_call({error, Error, Line}, _From, {Acc, Contents, File}) ->
    {reply, ok, {[{error, Error, Line}|Acc], Contents, File}};
handle_call({ok, Ok, Line}, _From, {Acc, Contents, File}) ->
    {reply, ok, {[{ok, Ok, Line}|Acc], Contents, File}}.


handle_cast(_, State) ->
    {ok, State}.

print_msg({warning, Warning, Line}, Contents, File) ->
    case Line of
	N when N =< length(Contents) ->
	    Src = lists:nth(Line, Contents),
	    io:format("~s:~p:WARN: ~s:~n  ~s~n~n", [File,Line,Warning,Src]);
	_ ->
	    ok
    end;
print_msg({error, Error, Line}, Contents, File) ->
    case Line of
	N when N =< length(Contents) ->
	    Src = lists:nth(Line, Contents),
	    io:format("~s:~p:Error: ~s:~n  ~s~n~n", [File,Line,Error,Src]);
	_ ->
	    ok
    end;
print_msg({ok, Ok, Line}, Contents, File) ->
    case Line of
	N when N =< length(Contents) ->
	    Src = lists:nth(Line, Contents),
	    io:format("~s:~p:OK: ~s:~n  ~s~n~n", [File,Line,Ok,Src]);
	_ ->
	    ok
    end.


get_lines(Device) ->
    case io:get_line(Device, "") of
	eof -> [];
	Line -> [string:trim(string:trim(Line, trailing, "\n"))|get_lines(Device)]
    end.
	     
