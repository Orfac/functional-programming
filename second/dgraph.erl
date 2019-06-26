-module(dgraph). 
-export([
   create_dgraph/2, create_edge/2, empty_dgraph/0,
   from/1, to/1, vertexes/1, edges/1, get_another_vertex/2,
   add_edge/2,delete_edge/2,
   merge_dgraphs/2, 
   foldl/3,foldr/3, filter_by_vertex/2,
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

merge_dgraphs(Graph1, Graph2) when Graph1 =:= Graph2 -> Graph1;
merge_dgraphs(Graph1, Graph2) ->
   Vertexes = merge_vertexes(vertexes(Graph1), vertexes(Graph2)),
   Edges = merge_edges(edges(Graph1),edges(Graph2)),
   create_dgraph(Vertexes,Edges).

merge_vertexes([], Vertexes2) -> Vertexes2;
merge_vertexes(Vertexes1, []) -> Vertexes1;
merge_vertexes(Vertexes1, Vertexes2) ->
   lists:append(
      Vertexes1, 
      lists:dropwhile(fun(X) -> lists:member(X,Vertexes1) end,Vertexes2)).

merge_edges([], Edges2) -> Edges2;
merge_edges(Edges1, []) -> Edges1;
merge_edges(Edges1, Edges2) ->
   lists:append(
      Edges1, 
      lists:dropwhile(fun(X) -> lists:member(X,Edges1) end,Edges2)).

add_edge(Edge, DGraph)->
   case (lists:member(from(Edge),vertexes(DGraph)) 
   and lists:member(to(Edge),vertexes(DGraph))
   and not(lists:member(Edge,edges(DGraph)))) of 
      true ->
         NewEdges = lists:append([Edge], edges(DGraph)),
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

get_another_vertex(Vertex, Edge) when Edge#edge.to =:= Vertex -> from(Edge);
get_another_vertex(_, Edge) -> to(Edge).

filter_by_vertex(_, DGraph) when (DGraph#dgraph.vertexes =:= []) -> DGraph;
filter_by_vertex(Fun, DGraph) ->
   Vertexes = lists:filter(Fun, vertexes(DGraph)),
   Edges = lists:filter(
      fun (Edge) -> 
      (lists:member(from(Edge), Vertexes)) 
      or (lists:member(to(Edge), Vertexes)) end,
      edges(DGraph)),
   create_dgraph(Vertexes, Edges).

foldl(_, Acc, DGraph) when (DGraph#dgraph.vertexes =:= []) -> Acc;
foldl(Fun, Acc, DGraph) when (length(DGraph#dgraph.vertexes) =:= 1) ->
   Vertex = lists:nth(1,vertexes(DGraph)),
   Fun(Vertex,Acc);
foldl(Fun, Acc, DGraph) ->
   [Vertex | Vertexes] = vertexes(DGraph),
   NewDGraph = create_dgraph(Vertexes, edges(DGraph)),
   foldl(Fun,Fun(Vertex,Acc),NewDGraph).

foldr(_, Acc, DGraph) when (DGraph#dgraph.vertexes =:= []) -> Acc;
foldr(Fun, Acc, DGraph) when (length(DGraph#dgraph.vertexes) =:= 1) ->
   Vertex = lists:nth(1,vertexes(DGraph)),
   Fun(Vertex,Acc);
foldr(Fun, Acc, DGraph) ->
   [Vertex | Vertexes] = vertexes(DGraph),
   NewDGraph = create_dgraph(Vertexes, edges(DGraph) ),
   Fun(Vertex, foldr(Fun, Acc, NewDGraph)).

print_edge(Edge) ->
   io:fwrite("From "),
   io:fwrite(integer_to_list(from(Edge))),
   io:fwrite(" To "),
   io:fwrite(integer_to_list(to(Edge))).

print_dgraph(DGraph) ->
   io:fwrite("Vertexes of dgraph~n"),
   lists:foreach(fun(Vertex) -> 
      io:fwrite(integer_to_list(Vertex)),
      io:fwrite(" ")end,
      vertexes(DGraph)),
   io:fwrite("~n"),
    io:fwrite("Edges of dgraph~n"),
   lists:foreach(fun(Edge) -> 
      print_edge(Edge),
      io:fwrite("~n")end,
      edges(DGraph)),
   io:fwrite("~n").