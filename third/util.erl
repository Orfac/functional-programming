-module(util). 
-export([parse_args/1, ask/1]).
-import(point, [str_to_float/1]).

% interpolation args 
parse_args(Args) when length(Args) =:= 1 ->
   [ Count_String | _ ] = Args,
   list_to_integer(atom_to_list(Count_String));

% approximation args
parse_args(Args) when length(Args) =:= 3 ->
   [Count , Step, Start_X] = Args,
   {list_to_integer(atom_to_list(Count)),
      str_to_float(atom_to_list(Step)),  
      str_to_float(atom_to_list(Start_X))}.

ask(Fun) ->
   Line = io:get_line(""),
   if 
      (Line =:= "stop\n") -> stop;
      true -> Fun(Line)
   end.