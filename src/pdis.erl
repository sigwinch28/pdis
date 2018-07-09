-module(pdis).

%% API exports
-export([main/1]).
-compile([export_all]).

-import(pdis_logger, [add_warning/2, add_error/2, add_info/2]).


-record(t_any,       {anno=[]}).
-record(t_union,     {anno=[], left :: type(), right:: type()}).
-record(t_singleton, {anno=[], value :: term(), type :: type()}).
-record(t_atom,      {anno=[]}).
-record(t_boolean,   {anno=[]}).
-record(t_pid,       {anno=[]}).
-record(t_tuple,     {anno=[], types :: [type()]}).
-record(t_none,      {anno=[]}).

-type t_any()       :: #t_any{}.
-type t_union()     :: #t_union{}.
-type t_singleton() :: #t_singleton{}.
-type t_atom()      :: #t_atom{}.
-type t_boolean()   :: #t_boolean{}.
-type t_pid()       :: #t_pid{}.
-type t_tuple()     :: #t_tuple{}.
-type t_none()      :: #t_none{}.

-type name() :: 'any' | 'union' | 'singleton' | 'atom' | 'boolean'
	      | 'pid' | 'tuple' | 'none'.

-type type() :: t_any() | t_union() | t_singleton() | t_atom()
	      | t_boolean() | t_pid() | t_tuple() | t_none().

type_eq(#t_any{}, #t_any{}) ->
    true;
type_eq(#t_union{left=L1, right=R1}, #t_union{left=L2, right=R2}) ->
    (type_eq(L1, L2) orelse type_eq(L1, R2))
	andalso
	  (type_eq(R1, R2) orelse type_eq(R1, L2));
type_eq(#t_singleton{value=V1, type=T1}, #t_singleton{value=V2, type=T2}) ->
    V1 =:= V2 andalso type_eq(T1, T2);
type_eq(#t_atom{}, #t_atom{}) ->
    true;
type_eq(#t_boolean{}, #t_boolean{}) ->
    true;
type_eq(#t_pid{}, #t_pid{}) ->
    true;
type_eq(#t_tuple{types=Ss}, #t_tuple{types=Ts}) ->
    case {Ss,Ts} of
	{undefined,undefined} -> true;
	{undefined,_} -> false;
	{_, undefined} -> false;
	_ ->
	    length(Ss) =:= length(Ts) andalso
		lists:all(fun(X) -> X =:= true end,
			  lists:zipwith(fun type_eq/2, Ss, Ts))
    end;
type_eq(#t_none{}, #t_none{}) ->
    true;
type_eq(_, _) ->
    false.

	

    

format_type(#t_any{}) -> "*";
format_type(#t_union{left=L, right=R}) ->
    [format_type(L)," | ",format_type(R)];
format_type(#t_singleton{value=V}) ->
    io_lib:format("~p", [V]);
format_type(#t_atom{}) ->
    "atom()";
format_type(#t_boolean{}) ->
    "boolean()";
format_type(#t_pid{}) ->
    "pid()";
format_type(#t_tuple{types=Types}) ->
    case Types of
	undefined ->
	    "tuple()";
	_ ->
	    ["{",lists:join(",",lists:map(fun format_type/1, Types)),"}"]
    end;
format_type(#t_none{}) ->
    "none()".

t_any_union() ->
    union_list([#t_atom{}, #t_boolean{}, #t_pid{}, #t_tuple{}]).

t_any() ->
    #t_any{}.

t_union(Left, Right) ->
    #t_union{left=Left, right=Right}.

t_singleton(Value, Type) ->
    #t_singleton{value=Value, type=Type}.

t_atom() ->
    #t_atom{}.

t_boolean() ->
    #t_boolean{}.

t_pid() ->
    #t_pid{}.

t_tuple(Types) ->
    #t_tuple{types=Types}.

t_none() ->
    #t_none{}.

%%====================================================================
%% API functions
%%====================================================================
scrape({Name, Body}) ->
    ReceiveTrees = find_receives(Body),
    SendTrees = find_receives(Body),
    
    FName = cerl:var_name(Name),
    {FName, SendTrees, ReceiveTrees}.

analyse({FName, SendTrees, ReceiveTrees}) ->
    Receives = lists:map(fun({Receive,Line}) -> {r(Receive),Line} end, ReceiveTrees),
    %Sends = lists:map(fun({Send,Line}) -> {s(Send),Line} end, SendTrees),
    Sends = [],
    {FName, Sends, Receives}.
			      
			
parse_type(t_any) ->	  
    #t_any{}.

%% escript Entry point
main(Args) ->
    pdis_logger:start_link(),
    [Filename|[Specfile|_Rest]] = Args,

    {ok, Specs} = file:consult(Specfile),

    {ok, Name, Module, _} = compile(Filename),
    pdis_logger:set_module(Name, Filename),

    Trees = lists:map(fun scrape/1, cerl:module_defs(Module)),
    
    StartTime = erlang:timestamp(),
    Types = lists:map(fun analyse/1, Trees),

    Res = lists:map(fun({FName, _Sends, Receives}) ->
			    case lists:keyfind(FName, 1, Specs) of
				false ->
				    add_warning("No spec for function ~p", [FName]),
				    ok;
				{_Name, TypeName} ->
				    FType = lists:foldl(
					      fun({TType,_Line}, Acc) ->
						      union(TType, Acc)
					      end, #t_none{}, Receives),
				    Type = parse_type(TypeName),
				    io:format("~p: spec: ~s, body: ~s~n", [FName, format_type(Type), format_type(FType)]),
				    case is_subtype(Type, FType) of
					true ->
					    ok;
					false ->
					    add_warning("Function ~p type does not match spec: ~p from spec vs ~s from body",
							[FName, format_type(Type), format_type(FType)]),
					    not_ok
				    end
			    end
		    end, Types),
    EndTime = erlang:timestamp(),
    Time = timer:now_diff(EndTime, StartTime),
    io:format("~n"),	 
    pdis_logger:print(),
    
    io:format("~n~ps~n", [Time / 1000000]),
    erlang:halt(0).

check({{_Dest, SendType},SLine}, Receives) ->
    lists:map(fun({Receive,RLine}) ->
		      {Res, Cls} = lists:foldl(fun(ReceiveType, {Res, Acc1}) ->
						       case is_subtype(SendType, ReceiveType) of
							   true ->
							       {true, [ReceiveType|Acc1]};
							   false ->
							       {Res, Acc1}
						       end
					       end, {false, []}, Receive),
		      %% case Res of
		      %% 	  false ->
		      %% 	      add_warning("No receive clause matches this send",SLine);
		      %% 	  _ ->
		      %% 	      ok
		      %% end,	  
		      {Receive, RLine, {Res, length(Cls)}}
	      end, Receives).


%%====================================================================
%% Internal functions
%%====================================================================

find_receives(Tree) ->
    cerl_trees:fold(fun(Node, Acc) ->
			    case cerl:type(Node) of
				'receive' -> [{Node,cerl_line(Node)}|Acc];
				_ -> Acc
			    end
		    end, [], Tree).

find_sends(Tree) ->
    cerl_trees:fold(fun(Node, Acc) ->
			    case cerl:type(Node) of
				'call' ->
				    Module = cerl:call_module(Node),
				    Name = cerl:call_name(Node),
				    case {cerl:type(Module), cerl:type(Name)} of
					{literal, literal} ->
					    ConcreteName = {cerl:concrete(Module),
							    cerl:concrete(Name)},
					    case ConcreteName of
						{'erlang','!'} ->
						    [{Node,cerl_line(Node)}|Acc];
						_ -> Acc
					    end;
					_ -> Acc
				    end;
				_ -> Acc
			    end
		    end, [], Tree).
	    
    

compile(Filename) ->
    compile:file(Filename, [binary,bin_opt_info,debug_info,report,return,verbose,to_core]).

%%====================================================================
%% Type of erlang terms (`:' operator)
%%====================================================================
type_of(X) when is_boolean(X) -> #t_boolean{};
type_of(X) when is_atom(X) -> #t_atom{};
type_of(X) when is_pid(X) -> #t_pid{};
type_of(X) when is_tuple(X) ->
    Types = lists:map(fun(Elem) ->
			      type_of(element(Elem,X))
		      end, lists:seq(1, size(X))),
    #t_tuple{types=Types};
type_of(_) -> #t_any{}. %% TODO: implement other type checks.

%%====================================================================
%% Type Checks
%%====================================================================
name_to_type(is_atom) -> #t_atom{};
name_to_type(is_pid) ->  #t_pid{};
name_to_type(is_boolean) -> #t_boolean{};
name_to_type(is_tuple) -> #t_tuple{}.

%%====================================================================
%% Typing Relation
%%====================================================================

is_subtype(#t_any{}, T) ->
    is_subtype(t_any_union(), T);
is_subtype(#t_none{}, _) ->
    true;
is_subtype(#t_union{left=S1, right=S2}, T) ->
    is_subtype(S1, T) andalso is_subtype(S2, T);
is_subtype(S, #t_union{left=T1, right=T2}) ->
    is_subtype(S, T1) orelse is_subtype(S, T2);
is_subtype(#t_singleton{value=Vs, type=S},#t_singleton{value=Vt, type=T}) ->
    (Vs == Vt) andalso is_subtype(S, T);
is_subtype(#t_singleton{type=S}, T) ->
    is_subtype(S, T);
is_subtype(#t_atom{}, #t_any{}) -> true;
is_subtype(#t_pid{}, #t_any{}) -> true;
is_subtype(#t_boolean{}, T) -> 
    case T of
	#t_atom{} -> true;
	_ -> is_subtype(#t_atom{}, T)
    end;
is_subtype(#t_tuple{types=undefined}, #t_any{}) ->
    true;
is_subtype(#t_tuple{types=Ss}, #t_tuple{types=Ts}) ->
    case Ss of
	undefined ->
	    Ts =:= undefined;
	_ ->
	    case Ts of
		undefined ->
		    true;
		_ ->
		    length(Ss) =:= length(Ts) andalso
			lists:all(fun(X) -> X =:= true end,
				  lists:zipwith(fun is_subtype/2, Ss, Ts))
	    end
    end;
is_subtype(#t_tuple{types=Ss}, T) ->
    case Ss of
	undefined ->
	    is_subtype(#t_tuple{}, #t_any{});
	_ ->
	    lists:all(fun(S) -> is_subtype(S, T) end, Ss)
    end;
is_subtype(S, T) ->
    type_eq(S, T).

%%====================================================================
%% Variable Mapping (vmap)
%%====================================================================
vmap(Pat) ->
    Id = fun(X) -> X end,
    maps:from_list(vmap1(Pat, Id)).

vmap1(Pat,F) ->
    case cerl:type(Pat) of
	literal ->
	    [];
	tuple ->
	    Es = cerl:tuple_es(Pat),
	    Count = length(Es),
	    Maps = lists:zipwith(
		     fun(Elem,N) ->
			     FElem = vmap_tuple_helper(Count, N, F),
			     vmap1(Elem,FElem)
		     end, Es, lists:seq(1, Count)),
	    lists:flatten(Maps);
	var ->
	    Name = cerl:var_name(Pat),
	    [{Name, F}]
    end.

vmap_tuple_helper(Size, Elem, F) ->
    fun(X) ->
	    Ts = lists:map(fun(N) ->
				   case N of
				       Elem -> F(X);
				       _ -> #t_any{}
				   end
			   end, lists:seq(1,Size)),
	    #t_tuple{types=Ts}
    end.

%%====================================================================
%% Receive Type Inference
%%====================================================================
r(Receive) ->
    Clauses = cerl:receive_clauses(Receive),
    Types = lists:map(fun c/1, Clauses),
    union_list(Types).
%    {_, CTypes} = lists:foldl(fun(Type, {AccType,Acc}) ->
%				      NewType = union(Type,AccType),
%				      {NewType, [NewType|Acc]}
%			      end, {#t_none{}, []}, Types),
%    lists:reverse(CTypes).

%%====================================================================
%% Clause Type Inference
%%====================================================================
c(Clause) ->
    [Pat] = cerl:clause_pats(Clause),
    Guard = cerl_guard_to_normal_form(cerl:clause_guard(Clause)),
    PType = p(Pat),
    Rho = vmap(Pat),
    GType = g(Guard, Rho),
    CType = intersect(PType, GType),
    case is_none(CType) of
	true ->
	    add_warning(
	      io_lib:format("This clause will never match as it has the type ~s",
			    [format_type(CType)]),
	      cerl_line(Clause));
	_ ->
	    ok
    end,
    CType.

%%====================================================================
%% Pattern Type Inference
%%====================================================================

p(Tree) ->
    case cerl:type(Tree) of
	literal ->
	    Value = cerl:concrete(Tree),
	    Type = type_of(Value),
	    #t_singleton{value=Value, type=Type};
	tuple ->
	    Es = cerl:tuple_es(Tree),
	    Types = lists:map(fun p/1, Es),
	    #t_tuple{types=Types};
	var ->
	    #t_any{}
    end.

%%====================================================================
%% Guard Type Inference
%%====================================================================
not_implemented(Node, Line) ->
    add_warning(io_lib:format("guard translation not implemented for ~p", [Node]), Line),
    #t_none{}.

g(Tree, Rho) ->
    Line = cerl_line(Tree),
    case cerl:type(Tree) of
	literal ->
	    case cerl:concrete(Tree) of
		true -> #t_any{};
		false -> #t_none{}
	    end;
	call ->
	    RawName = cerl:call_name(Tree),
	    Name = case cerl:type(RawName) of
		       literal -> cerl:concrete(RawName);
		       _ ->
			   not_implemented(RawName, Line)
		   end,
	    case Name of
		'and' ->
		    [G1, G2] = cerl:call_args(Tree),
		    T1 = g(G1, Rho),
		    T2 = g(G2, Rho),
		    Res = intersect(T1, T2),
		    case is_none(Res) of
			true ->
			    add_warning(
			      io_lib:format("cannot intersect types ~s and ~s in this guard",
					    [format_type(T1), format_type(T2)]),
			      Line);
			false -> ok
		    end,
		    Res;
		'or' ->
		    [L, R] = cerl:call_args(Tree),
		    union(g(L,Rho), g(R, Rho));
		Other ->
		    case name_to_type(Other) of
			undefined ->
			    add_warning(
			      io_lib:format("Unrecognised call name: ~w", [Other]),
			      Line),
			    #t_any{};
			T ->
			    [Arg] = cerl:call_args(Tree),
			    V = cerl:var_name(Arg),
			    F = maps:get(V, Rho),
			    F(T)
		    end
	    end;
	'case' ->
	    case cerl_case_to_bool(Tree) of
		{true, G1, G2, G3} ->
		    union(intersect(g(G1, Rho), g(G2, Rho)), g(G3, Rho));
		_ ->
		    add_warning("Unrecognised case statement", Line),
		    #t_any{}
	    end
    end.

%%====================================================================
%% Send analysis
%%====================================================================
s(Send) ->
    [Dest,Content] = cerl:call_args(Send),
    SType = p(Content),
    {Dest, SType}.

is_none(#t_none{}) ->
    true;
is_none(#t_union{left=L, right=R}) ->
    is_none(L) andalso is_none(R);
is_none(#t_tuple{types=Ts}) ->
    case Ts of
	undefined ->
	    false;
	_ ->
	    lists:any(fun is_none/1, Ts)
    end;
is_none(_) ->
    false.



intersect(#t_union{left=S1, right=S2}, T) ->
    #t_union{left=intersect(S1, T), right=intersect(S2, T)};
intersect(S, #t_union{left=T1, right=T2}) ->
    #t_union{left=intersect(S, T1), right=intersect(S, T2)};
intersect(#t_tuple{types=Ss} = L, #t_tuple{types=Ts} = R) ->
    case Ss of
	undefined ->
	    R;
	_ ->
	    case Ts of
		undefined ->
		    L;
		_ ->
		       case length(Ss) =:= length(Ts) of
			   true ->
			       Res = lists:zipwith(fun intersect/2, Ss, Ts),
			       #t_tuple{types=Res};
			   false ->
			       #t_none{}
		       end
	    end
    end;
intersect(S, T) ->
    case is_subtype(S, T) of
	true -> S;
	false ->
	    case is_subtype(T, S) of
		true -> T;
		false -> #t_none{}
	    end
    end.     

union(S, T) ->
    #t_union{left=S, right=T}.

union_list(Types) ->
    lists:foldl(fun union/2, #t_none{}, Types).

%%====================================================================
%% Core Erlang AST helpers
%%====================================================================
cerl_line(Tree) ->
    case cerl:get_ann(Tree) of
	[N|_] when is_number(N) ->
	    N;
	_ ->
	    undefined
    end.

cerl_is_compiler_generated(Tree) ->
    lists:member(compiler_generated, cerl:get_ann(Tree)).

cerl_guard_to_normal_form(Tree) ->
    cerl_trees:map(fun cerl_normalise_guard/1, Tree).

cerl_normalise_guard(Tree) ->
    case cerl:type(Tree) of
	'call' ->
	    case cerl_is_compiler_generated(Tree) of
		true ->
		    Module = cerl:concrete(cerl:call_module(Tree)),
		    Name = cerl:concrete(cerl:call_name(Tree)),
		    case {Module, Name} of
			{erlang, '=:='} ->
			    [L, R] = cerl:call_args(Tree),
			    case cerl:type(L) =:= literal andalso
				cerl:concrete(L) =:= true of
				true ->
				    R;
				false ->
				    literal = cerl:type(R),
				    L
			    end;
			_ -> Tree
		    end;
		false -> Tree
	    end;
	'let' ->
	    case cerl:let_vars(Tree) of
		[Var] ->
		    Res = cerl_subst(
		      cerl:var_name(Var),
		      cerl:let_arg(Tree),
		      cerl:let_body(Tree)),
		    Res;
		_ ->
		    Tree
	    end;
	'try' ->
	    cerl:try_arg(Tree);
	_ -> Tree
    end.

-spec cerl_subst(atom(), cerl:cerl(), cerl:cerl()) -> cerl:cerl().
cerl_subst(VName, Body, Tree) ->
    cerl_trees:map(fun(Node) ->
                           case cerl:type(Node) of
                               'var' ->
                                   case cerl:var_name(Node) of
                                       VName ->
                                           Body;
                                       _ -> Node
                                   end;
                               _ -> Node
                           end
                   end, Tree).


cerl_case_to_bool(Tree) ->
    case cerl:case_clauses(Tree) of
	[True, False, Catch] ->
	    TrueLit = is_cerl_clause_pats_literal(True),
	    FalseLit = is_cerl_clause_pats_literal(False),
	    IsCatch = is_cerl_clause_catch_all(Catch),
	    case {TrueLit, FalseLit, IsCatch} of
		{{true, true}, {true, false}, true} ->
		    Arg = cerl:case_arg(Tree),
		    TrueBody = cerl:clause_body(True),
		    FalseBody = cerl:clause_body(False),
		    {true, Arg, TrueBody, FalseBody};
		_ -> false
	    end;
	_ -> false
    end.
	    
is_cerl_clause_pats_literal(Tree) ->
    case cerl:clause_pats(Tree) of
	[Pat] ->
	    case cerl:type(Pat) of
		literal ->
		    {true, cerl:concrete(Pat)};
		_ ->
		    false
	    end;
	_ -> false
    end.

is_cerl_clause_catch_all(Tree) ->
    case cerl:clause_pats(Tree) of
	[Pat] ->
	    Pat =:= cerl:clause_body(Tree);
	_ -> false
    end.
