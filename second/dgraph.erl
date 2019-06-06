-module(dgraph). 
-export([
   create_dgraph/2, create_edge/2, empty_dgraph/0,
   from/1, to/1, vertexes/1, edges/1, get_another_vertex/2,
   add_edge/2,delete_edge/2,
   merge_dgraphs/2, compare_edges/2,
   print_dgraph/1, print_edge/1]). 

-record(edge, {from, to}).
-record(dgraph, {vertexes, edges}).

from(Edge) -> Edge#edge.from.
to(Edge) -> Edge#edge.to.
vertexes(DGraph) -> DGraph#dgraph.vertexes.
edges(DGraph) -> DGraph#dgraph.edges.

create_edge(From, To) -> 
   #edge{from = From, to = To}.

create_dgraph(Vertexes,Edges) ->
   #dgraph{vertexes = Vertexes, edges = Edges}.

empty_dgraph() ->
    #dgraph{vertexes = [], edges = []}.

compare_edges(A, B) ->
    case A#edge.from == B#edge.from of
        true ->
            A#edge.to > B#edge.to;
        _ ->
            A#edge.from > B#edge.from
    end.
   

merge_dgraphs(Graph1, Graph2) ->
   Vertexes = merge_vertexes(Graph1#dgraph.vertexes,Graph2#dgraph.vertexes),
   Edges = merge_edges(Graph1#dgraph.edges,Graph2#dgraph.edges),
   create_dgraph(Vertexes,Edges).

merge_vertexes([], Vertexes2) -> Vertexes2;
merge_vertexes(Vertexes1, []) -> Vertexes1;
merge_vertexes(Vertexes1, Vertexes2) ->
   lists:sort(lists:append(Vertexes1,Vertexes2)).

merge_edges([], Edges2) -> Edges2;
merge_edges(Edges1, []) -> Edges1;
merge_edges(Edges1, Edges2) ->
   lists:sort(fun compare_edges/2,lists:append(Edges1,Edges2)).

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
   NewEdges = lists:filter(
      fun ([ _ | Edge]) -> 
      (from(Edge) =:= Vertex) 
      or (to(Edge) =:= Vertex) end,
      edges(DGraph)),
   NewVertexes = lists:map(
      fun (Edge) -> get_another_vertex(Vertex, Edge) end,
      NewEdges),
   create_dgraph(NewVertexes, NewEdges).

print_edge(Edge) ->
   io:fwrite("From "),
   io:fwrite(integer_to_list(Edge#edge.from)),
   io:fwrite(" To "),
   io:fwrite(integer_to_list(Edge#edge.to)).

print_dgraph(DGraph) ->
   io:fwrite("Vertexes of dgraph~n"),
   lists:foreach(fun(Vertex) -> 
      io:fwrite(integer_to_list(Vertex)),
      io:fwrite(" ")end,
      DGraph#dgraph.vertexes),
   io:fwrite("~n"),
    io:fwrite("Edges of dgraph~n"),
   lists:foreach(fun(Edge) -> 
      print_edge(Edge),
      io:fwrite("~n")end,
      DGraph#dgraph.edges),
   io:fwrite("~n").