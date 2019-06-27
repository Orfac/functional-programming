-module(main). 
-export([main/1, listen_interpolater/0, listen_reciever/0]).
-import(interpolation,[interpolate/2, create_point/2]).

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


fill_point(Points, Pids) ->
   Line = io:get_line(""),
   if 
      (Line =:= "stop\n") -> ok;
      true -> 
         X_Tuple = [string:to_float(lists:droplast(Line))],
         [{X,_}] = X_Tuple,
         [Interpolater | Receiver] = Pids,
         Interpolater ! {calculate, Receiver, Points, X},
         fill_point(Points,Pids)
   end.

fill_start_point(Max_N, Max_N, Points, Pids) -> fill_point(Points, Pids);
fill_start_point(N, Max_N, Points, Pids) ->
   Line = io:get_line(""),
   if 
      (Line =:= "stop\n") -> ok;
      true -> 
         Point_Tokens = string:tokens(Line, ";"),
         [ X_String | Y_String ] = Point_Tokens,
         X_Tuple = [string:to_float(X_String)],
         [{X,_}] = X_Tuple,
         Y_Tuple = [string:to_float(lists:droplast(Y_String))],
         [{Y,_}] = Y_Tuple,
         Point = create_point(X,Y),
         fill_start_point(N + 1, Max_N,[Point | Points], Pids)
   end.


main(Args) ->
   Pid = spawn(fun listen_interpolater/0),
   Pid2 = spawn(fun listen_reciever/0),
   io:format("Args: ~p\n", [Args]),
   fill_start_point(5,6,[],[Pid,Pid2]).