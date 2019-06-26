-module(unit). 
-import(dgraph,[create_dgraph/2, create_edge/2, empty_dgraph/0,
   from/1, to/1, vertexes/1, edges/1, get_another_vertex/2,
   add_edge/2,delete_edge/2,merge_dgraphs/2, compare_edges/2,
   filter_by_vertex/2,
   foldl/3,foldr/3,
    print_dgraph/1, print_edge/1]).
-export([start/0]). 
-include("dgraph.hrl").
-include_lib("eunit/include/eunit.hrl").

create_test_dgraph(X) ->
    Edges = [create_edge(1,Y) || Y <- lists:seq(1,X)],
    Vertexes = lists:seq(1,X),
    create_dgraph(Vertexes,Edges).

create_test_dgraph_without_edges(X) ->
    Vertexes = lists:seq(1,X),
    create_dgraph(Vertexes, []).

edge_addition_increases_count_of_edges_in_dgraph_test() ->
    Dgraph = create_test_dgraph_without_edges(3),
    Edge = create_edge(1,2),
    NewDGraph = add_edge(Edge, Dgraph),
    ?assert(1 =:= length(edges(NewDGraph))).

edge_addition_of_existed_edge_doesnt_changes_count_of_edges_in_dgraph_test() ->
    Dgraph = create_test_dgraph_without_edges(3),
    Edge = create_edge(1,2),
    NewDGraph = add_edge(Edge, Dgraph),
    NewDGraph2 = add_edge(Edge, NewDGraph),
    ?assert(1 =:= length(edges(NewDGraph2))).

edge_deletion_deletes_edge_from_dgraph_test() ->
    Dgraph = create_test_dgraph(3),
    Edge = create_edge(1,2),
    NewDGraph = delete_edge(Edge, Dgraph),
    ?assert(2 =:= length(edges(NewDGraph))).

equality_of_left_and_right_merges_test() ->
    Dgraph1 = create_test_dgraph(4),
    Dgraph2 = create_test_dgraph(4),
    Dgraph3 = merge_dgraphs(Dgraph1,Dgraph2),
    Dgraph4 = merge_dgraphs(Dgraph2,Dgraph1),
    ?assert(Dgraph3 =:= Dgraph4).

left_fold_works_test() ->
    Dgraph = create_test_dgraph(5),
    Sum = foldl(fun(X, Acc) -> X + Acc end, 0, Dgraph),
    ?assert(Sum =:= 15).

right_fold_works_test() ->
    Dgraph = create_test_dgraph(5),
    Sum = foldr(fun(X, Acc) -> X + Acc end, 0, Dgraph),
    ?assert(Sum =:= 15).

filter_works_test() ->
    Dgraph = create_test_dgraph(4),
    NewDgraph = filter_by_vertex(fun(X) -> X =:= 2 end,Dgraph),
    ?assert(1 =:= length(edges(NewDgraph))).

empty_dgraph_dont_change_dgraph_after_merge_test() ->
    Dgraph1 = create_test_dgraph(5),
    Dgraph2 = merge_dgraphs(Dgraph1, empty_dgraph()),
    ?assert(Dgraph1 =:= Dgraph2).

start() -> unit:test().