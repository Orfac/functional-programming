-module(unit). 
-import(dgraph,[create_dgraph/2, create_edge/2, empty_dgraph/0,
   from/1, to/1, vertexes/1, edges/1, get_another_vertex/2,
   add_edge/2,delete_edge/2,merge_dgraphs/2, compare_edges/2,
    print_dgraph/1, print_edge/1]).
-export([start/0]). 
-include("dgraph.hrl").
-include_lib("eunit/include/eunit.hrl").

create_test_dgraph(X) ->
    Edges = [create_edge(1,Y) || Y <- lists:seq(1,X)],
    Vertexes = lists:seq(1,X),
    create_dgraph(Vertexes,Edges).

left_merge_equals_to_right_merge_test() ->
    Dgraph1 = create_test_dgraph(4),
    Dgraph2 = create_test_dgraph(4),
    Dgraph3 = merge_dgraphs(Dgraph1,Dgraph2),
    Dgraph4 = merge_dgraphs(Dgraph2,Dgraph1),
    ?assert(Dgraph3 =:= Dgraph4).

empty_dgraph_dont_change_dgraph_after_merge_test() ->
    Dgraph1 = create_test_dgraph(5),
    Dgraph2 = merge_dgraphs(Dgraph1, empty_dgraph()),
    ?assert(Dgraph1 =:= Dgraph2).

start() -> unit:test().