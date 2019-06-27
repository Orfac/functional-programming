-module(main). 
-export([main/1]).
-import(point, [x/1, y/1, create_point/2, str_to_point/1, str_to_ordinate/1]).
-import(interpolation,[interpolate/2]).

listen_interpolater() ->
   receive
      {calculate, Pid, Points, X} ->
         Pid ! {result, X, interpolate(Points,X)},
         listen_interpolater();
      {stop} ->
         io:format("Stopping interpolater~n");
      Other ->
         io:format("Unknown: ~p~n", [Other]),
         listen_interpolater()
   end.

listen_reciever() ->
   receive 
      {result, X, Y} ->
         io:format("The result is: ~f ~f~n",[X,Y]),
         listen_reciever();
      {stop} -> 
         io:format("Stopping receiver~n");
      Other ->
         io:format("Unknown: ~p~n", [Other]),
         listen_reciever()
   end.


get_y(Points, Pids) ->
   Line = io:get_line(""),
   if 
      (Line =:= "stop\n") -> ok;
      true -> 
         X = str_to_ordinate(Line),
         [Interpolater | Receiver] = Pids,
         Interpolater ! {calculate, Receiver, Points, X},
         get_y(Points,Pids)
   end.

fill_start_point(N, Max_N, Points, Pids) when N =:= Max_N -> 
   io:format("Points have been accepted\n"),
   get_y(Points, Pids);

fill_start_point(N, Max_N, Points, Pids) ->
   Line = io:get_line(""),
   if 
      (Line =:= "stop\n") -> ok;
      true ->    
         Point = str_to_point(Line),
         io:format("The point is: ~f ~f~n",[x(Point),y(Point)]),
         fill_start_point(N + 1, Max_N,[Point | Points], Pids)
   end.


main(Args) ->
   Pid = spawn(fun listen_interpolater/0),
   Pid2 = spawn(fun listen_reciever/0),
   io:format("Args: ~p\n", [Args]),
   fill_start_point(1,6,[],[Pid | Pid2]),
   Pid ! {stop},
   Pid2 ! {stop}.