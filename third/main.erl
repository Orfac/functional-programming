-module(main). 
-export([main/1, listen_interpolater/0, listen_reciever/0]).
-import(interpolation,[interpolate/2]).

listen_interpolater() ->
   receive
      {calculate, Pid, Points, X} ->
         Pid ! {result, X, interpolate(Points,X)},
         listen_interpolater();
      {stop} ->
         io:format("Stopping~n");
      Other ->
         io:format("Unknown: ~p~n", [Other]),
         listen_interpolater()
   end.

listen_reciever() ->
   receive 
      {result, X, Y} ->
         io:format("The result is: ~f ~f", X,Y),
         listen_reciever();
      {stop} -> 
         io:format("Stopping receiver~n");
      Other ->
         io:format("Unknown: ~p~n", [Other]),
         listen_reciever()
   end.

main(Args) ->
   io:format("Args: ~p\n", [Args]),
   Term = io:get_line(""),
   Term2 = io:get_line(""),
   io:format(Term), io:format(Term2).