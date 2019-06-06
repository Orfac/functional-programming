-module(unit). 
-import(dgraph,[create_dgraph/2, create_edge/2, empty_dgraph/0,
   from/1, to/1, vertexes/1, edges/1, get_another_vertex/2,
   add_edge/2,delete_edge/2, max_edge/2, min_edge/2,
   merge_dgraphs/2,merge_vertexes/4,merge_vertexes/2, merge_edges/4, merge_edges/2,
    print_dgraph/1, print_edge/1]).
-export([start/0]). 

-include_lib("eunit/include/eunit.hrl").

generate_random_dgraph(N) ->
    Vertexes = [rand:uniform(10) 
            || _ <- lists:seq(1, N)],
    Edges = [create_edge(rand:uniform(10), rand:uniform(10))
            || _ <- lists:seq(1, N)],
    create_dgraph(Vertexes,Edges).

create_test_dgraph(X) ->
    Edges = [create_edge(1,Y) || Y <- lists:seq(1,X)],
    Vertexes = lists:seq(1,X),
    create_dgraph(Vertexes,Edges).

left_merge_equals_to_right_merge_test() ->
    Dgraph1 = generate_random_dgraph(4),
    Dgraph2 = generate_random_dgraph(4),
    print_dgraph(Dgraph1),
    print_dgraph(Dgraph2),
    Dgraph3 = merge_dgraphs(Dgraph1,Dgraph2),
    print_dgraph(Dgraph3),
    Dgraph4 = merge_dgraphs(Dgraph2,Dgraph1),
    ?assert(Dgraph3 =:= Dgraph4).

empty_dgraph_dont_change_dgraph_after_merge_test() ->
    Dgraph1 = generate_random_dgraph(5),
    Dgraph2 = merge_dgraphs(Dgraph1, empty_dgraph()),
    Dgraph3 = merge_dgraphs(empty_dgraph(), Dgraph1),
    ?assert(Dgraph1 =:= Dgraph2),
    ?assert(Dgraph1 =:= Dgraph3).

reverse_test() -> lists:reverse([1,2,3]).
length_test() -> ?assert(length([1,2,3]) =:= 3).

start() -> 
    
    unit:test(),
    io:fwrite("~n").