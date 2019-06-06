-module(prop). 
-import(dgraph,[create_dgraph/2,create_edge/2,
add_edge/2,delete_edge/2,vertexes/1,edges/1,from/1,to/1, get_another_vertex/2]).
-export([start/0]). 

-include_lib("proper/include/proper.hrl").

print_int(Number) ->
   io:fwrite(integer_to_list(Number)).

start() -> 
   %io:fwrite("Proper-based tests are running:~n"),
   io:fwrite("Unit tests are running:~n"),
   main:test(),
   io:fwrite("~n").