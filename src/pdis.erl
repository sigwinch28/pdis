-module(pdis).

%% API exports
-export([main/1]).
-compile([export_all]).


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
    ["{",lists:join(",",lists:map(fun format_type/1, Types)),"}"];
format_type(#t_none{}) ->
    "none()".

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    [Filename|_] = Args,
    io:format("Filename: ~p~n", [Filename]),

    {ok, Name, Module, _} = compile(Filename),
    io:format("Module name: ~p~n", [Name]),

    io:format("---~n~p~n---~n", [Module]),

    ReceiveTrees = find_receives(Module),

    Receives = lists:map(fun r/1, ReceiveTrees),
    io:format("~p~n", Receives),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

find_receives(Tree) ->
    cerl_trees:fold(fun(Node, Acc) ->
			    case cerl:type(Node) of
				'receive' -> [Node|Acc];
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
						    [Node|Acc];
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
type_of(X) when is_pid(X) -> #t_pid{}.

%%====================================================================
%% Type Checks
%%====================================================================
name_to_type(is_atom) -> #t_atom{};
name_to_type(is_pid) ->  #t_pid{};
name_to_type(is_boolean) -> #t_boolean{};
name_to_type(_) -> undefined.

%%====================================================================
%% Typing Relation
%%====================================================================

is_subtype(T, T) -> true;
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
	_ -> is_subtype(atom, T)
    end;
is_subtype(#t_tuple{types=Ss}, any) ->
    lists:all(fun(S) -> is_subtype(S, any) end, Ss);
is_subtype(#t_tuple{types=Ss}, #t_tuple{types=Ts}) ->
    length(Ss) =:= length(Ts) andalso
	lists:all(fun(X) -> X =:= true end,
		  lists:zipwith(fun is_subtype/2, Ss, Ts));
is_subtype(_, _) ->
    false.

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
    RType = lists:foldl(fun union/2, #t_any{}, Types),
    io:format("RType: ~s~n", [format_type(RType)]),
    RType.

%%====================================================================
%% Clause Type Inference
%%====================================================================
c(Clause) ->
    [Pat] = cerl:clause_pats(Clause),
    Guard = cerl_guard_to_normal_form(cerl:clause_guard(Clause)),
    io:format("---Guard---~n~p~n---~n", [Guard]),
    PType = p(Pat),
    io:format("PType: ~s~n", [format_type(PType)]),
    Rho = vmap(Pat),
    GType = g(Guard, Rho),
    io:format("GType: ~s~n", [format_type(GType)]),
    
    CType = intersect(PType, GType),
    io:format("CType: ~s~n", [format_type(CType)]),
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
g(Tree, Rho) ->
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
		       _ -> throw({error, not_implemented, Tree})
		   end,
	    case Name of
		'and' ->
		    [G1, G2] = cerl:call_args(Tree),
		    intersect(g(G1, Rho), g(G2, Rho));
		'or' ->
		    [L, R] = cerl:call_args(Tree),
		    union(g(L,Rho), g(R, Rho));
		Name ->
		    case name_to_type(Name) of
			undefined ->
			    throw({error, not_implemented, Tree});
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
		_ -> throw({error, not_implemented, Tree})
	    end
    end.

intersect(#t_tuple{types=Ss}, #t_tuple{types=Ts}) ->
    case length(Ss) =:= length(Ts) of
	true ->
	    Res = lists:zipwith(fun intersect/2, Ss, Ts),
	    #t_tuple{types=Res};
	false ->
	    #t_none{}
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

%%====================================================================
%% Core Erlang AST helpers
%%====================================================================
cerl_guard_to_normal_form(Tree) ->
    cerl_trees:map(fun cerl_normalise_guard/1, Tree).

cerl_normalise_guard(Tree) ->
    case cerl:type(Tree) of
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
