-module(main). 
-export([main/1]).
-import(point, [x/1, y/1, create_point/2, str_to_point/1, str_to_float/1]).
-import(interpolation,[interpolate/2]).
-import(util, [parse_args/1, ask/1]).
-import(approximation,[approximate/2]).

listen_interpolator() ->
   receive
      {calculate, Pid, Points, X} ->
         Pid ! {result, X, interpolate(Points,X)},
         listen_interpolator();
      {stop} ->
         io:format("Stopping interpolator~n");
      Other ->
         io:format("Unknown: ~p~n", [Other]),
         listen_interpolator()
   end.

listen_approximator() ->
   receive
      {calculate, Pid, Points, X} ->
         Pid ! {result, X, approximate(Points,X)},
         listen_approximator();
      {stop} ->
         io:format("Stopping interpolator~n");
      Other ->
         io:format("Unknown: ~p~n", [Other]),
         listen_approximator()
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

call_for_calculate(Points, X, Pids) ->
   [Interpolator | Receiver] = Pids,
   Interpolator ! {calculate, Receiver, Points, X}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Approximation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
approximate_loop(0, _, _, _, _) -> ok;
approximate_loop(Count, Step, X, Points, Pids) when length(Points) < 1 ->
   Point = ask(fun(Line) -> str_to_point(Line) end),
   if (Point =:= stop) -> ok;
   true -> 
      approximate_loop(Count, Step, X, [Point], Pids)
   end;
   
approximate_loop(Count, Step, X, Points, Pids) when length(Points) =:= 1 ->
   [ Last_Point | _] = Points,
   Point = ask(fun(Line) -> str_to_point(Line) end),
   if (Point =:= stop) -> ok;
   true ->
      approximate_loop(Count, Step, X, [ Point | Last_Point ], Pids)
   end;

approximate_loop(Count, Step, X, Points, Pids) ->
   [Last_Point | _] = Points,
   Last_X = x(Last_Point),
   if (Last_X > X) ->
      call_for_calculate(Points, X, Pids),
      approximate_loop(Count - 1, Step, X + Step, Points, Pids);
   true ->
      Point = ask(fun(Line) -> str_to_point(Line) end),
      if (Point =:= stop) -> ok;
      true -> 
         approximate_loop(Count, Step, X, [Point | Last_Point], Pids)
      end
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interpolation 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fill_start_points(0, Points) -> Points;
fill_start_points(N, Points) ->
   Point = ask(fun(Line) -> str_to_point(Line) end),
   if (Point =:= stop) -> ok;
   true -> fill_start_points(N - 1, [Point | Points])
   end.

interpolate_loop(Points, Pids) ->
   X = ask(fun(Line) -> str_to_float(string:trim(Line)) end),
   if (X =:= stop) -> ok;
   true ->
   call_for_calculate(Points, X, Pids),
   interpolate_loop(Points,Pids)
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main(Args) ->
   io:format("Taken args: ~p\n", [Args]),

   Receiver = spawn(fun listen_reciever/0),

   if (length(Args) =:= 3 ) ->
      { Count, Step, Start_X } = parse_args(Args),
      Interpolator = spawn(fun listen_approximator/0),
      io:format("Input point in format\n 1f;1f\n E.g. 15.7;-16\n"),
      approximate_loop(Count, Step, Start_X, [], [Interpolator | Receiver]);
   true ->
      Count = parse_args(Args),
      io:format("Input ~w point(s) in format\n 1f;1f\n E.g. 15.7;-16\n", [Count]),
      Points = fill_start_points(Count,[]),
      io:format("Input X in format\n 1f\n E.g. 15.7\n"),
      Interpolator = spawn(fun listen_interpolator/0),
      interpolate_loop(Points,[Interpolator | Receiver])
   end,
   
   Interpolator ! {stop},
   Receiver ! {stop}.