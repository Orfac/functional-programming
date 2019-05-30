-module(main). 
-export([start/0,create_dgraph/2,create_edge/2,
add_edge/2,delete_edge/2,vertexes/1,edges/1,from/1,to/1, get_another_vertex/2, sort/1]). 

-include_lib("proper/include/proper.hrl").



-record(edge, {from, to}).
-record(dgraph, {vertexes, edges}).
sort([]) -> [];
sort([P|Xs]) ->
    sort([X || X <- Xs, X < P]) ++ [P] ++ sort([X || X <- Xs, P < X]).
prop_ordered() ->
    ?FORALL(L, integer(), ordered(sort(L))).

ordered([]) -> true;
ordered([_]) -> true;
ordered([A,B|T]) -> A =< B andalso ordered([B|T]).

from(Edge) -> Edge#edge.from.
to(Edge) -> Edge#edge.to.
vertexes(DGraph) -> DGraph#dgraph.vertexes.
edges(DGraph) -> DGraph#dgraph.edges.

create_edge(From, To) -> 
   #edge{from = From, to = To}.

create_dgraph(Vertexes,Edges) ->
   #dgraph{vertexes = Vertexes, edges = Edges}.

add_edge(Edge, DGraph)->
   case (lists:member(from(Edge),vertexes(DGraph)) 
   and lists:member(to(Edge),vertexes(DGraph))) of 
      true ->
         NewEdges = lists:append(Edge, edges(DGraph)),
         create_dgraph(vertexes(DGraph),NewEdges);
      false -> DGraph
   end.

delete_edge(Edge, DGraph)-> 
   OldEdges = edges(DGraph),
   NewEdges = lists:delete(Edge,OldEdges),
   case NewEdges of
      OldEdges -> DGraph;
      ChangedEdges -> 
         NewDGraph = create_dgraph(vertexes(DGraph),ChangedEdges),
         delete_edge(Edge,NewDGraph)
   end.

get_another_vertex(Vertex, Edge) ->
   case from(Edge) of 
         Vertex -> to(Edge);
         AnotherVertex -> AnotherVertex
   end.

filter_by_vertex(Vertex, DGraph) ->
   NewEdges = lists:filter(fun ([ _ | Edge]) -> 
      (from(Edge) =:= Vertex) or (to(Edge) =:= Vertex) end,
      edges(DGraph)),
   NewVertexes = lists:map(fun (Edge) ->
      get_another_vertex(Vertex, Edge)
         end,
      NewEdges),
   create_dgraph(NewVertexes, NewEdges).


print_int(Number) ->
   io:fwrite(integer_to_list(Number)).

start() -> 
   Edge = create_edge(1,5), 
   proper:quickcheck(main:prop_ordered()),
   
   print_int(Edge#edge.to),
   io:fwrite("~n").