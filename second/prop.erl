-module(prop). 
-import(dgraph,[create_dgraph/2, create_edge/2, empty_dgraph/0,
   from/1, to/1, vertexes/1, edges/1, get_another_vertex/2, add_vertex/2,
   add_edge/2,delete_edge/2,merge_dgraphs/2, compare_edges/2,
    print_dgraph/1, print_edge/1]).
-export([start/0, generate_random_dgraph/0,
         merging_empty_dgraph_with_dgraph_returns_same_dgraph/0,
         merging_dgraph_with_empty_dgraph_returns_same_dgraph/0,
         merging_dgraphs_is_associative/0]). 

-include_lib("proper/include/proper.hrl").

generate_random_dgraph() ->
   Vertexes = [ X || X <- lists:seq(1, rand:uniform(10))],
   Dgraph = create_dgraph(Vertexes, []),
   lists:foldr(
      fun(X, Acc) ->
         Edge = create_edge(X,rand:uniform(10)),
         add_edge(Edge, Acc) end,
      Dgraph,
      Vertexes).

merging_empty_dgraph_with_dgraph_returns_same_dgraph() ->
   ?FORALL(
      Dgraph,
      generate_random_dgraph(),
      Dgraph =:= merge_dgraphs(empty_dgraph(),Dgraph)
   ).

merging_dgraph_with_empty_dgraph_returns_same_dgraph() ->
   ?FORALL(
      Dgraph,
      generate_random_dgraph(),
      Dgraph =:= merge_dgraphs(Dgraph,empty_dgraph())
   ).

check_associative_addition(Dgraph) ->
   
   Dgraph2 = generate_random_dgraph(),
   Dgraph3 = generate_random_dgraph(),
   ResultDgraph1 = merge_dgraphs(merge_dgraphs(Dgraph,Dgraph2),Dgraph3),
   ResultDgraph2 = merge_dgraphs(Dgraph,merge_dgraphs(Dgraph2,Dgraph3)),
  
   if (ResultDgraph1 =:= ResultDgraph2) -> true;
      true -> 
          io:format("~n NEW"),
      print_dgraph(Dgraph),
      print_dgraph(Dgraph2),
      print_dgraph(Dgraph3),
      print_dgraph(ResultDgraph1),
      print_dgraph(ResultDgraph2),
      ResultDgraph1 =:= ResultDgraph2
   end.  

merging_dgraphs_is_associative() ->
   ?FORALL(
      Dgraph,
      generate_random_dgraph(),
      check_associative_addition(Dgraph)
   ).


start() -> 
   %io:fwrite("Proper-based tests are running:~n"),
   proper:quickcheck(prop:merging_empty_dgraph_with_dgraph_returns_same_dgraph()),
   proper:quickcheck(prop:merging_dgraph_with_empty_dgraph_returns_same_dgraph()),
   proper:quickcheck(prop:merging_dgraphs_is_associative()),
   io:fwrite("~n").