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


fill_start_point([Points]) ->
   Line = io:get_line(""),
   if 
      (Line =:= "stop\n") -> ok;
      true -> 
         Point_Tokens = string:tokens(Line, ";"),

         
X = 
Y = 
         Point = create_point(X,Y).
         fill_start_point([Point | Points])
   end.

fill_point() ->
   
   ok.

main(Args) ->
   % Pidasdasd = spawn(fun listen_receiver/0)
   io:format("Args: ~p\n", [Args]),
   fill_start_point([]).