-module(main).  
-export([for/3,isPrime/1,isPrime/3,start/0]). 

isPrime(1) -> false;
isPrime(2) -> true;
isPrime(N) -> isPrime(N, 2, N div 2).

isPrime(_, Divider, MaxDivider) when (Divider > MaxDivider) -> true;
isPrime(N, Divider, _) when (N rem Divider == 0) -> false;
isPrime(N, Divider, MaxDivider) -> isPrime(N, Divider + 1, MaxDivider).

for(I,Count,N) -> 
   case (isPrime(I)) of
   true ->   
      Count2 = Count + 1,
      if (Count2 =:= N) -> I;
      true -> for(I + 1, Count2, N)
      end;
   false -> for(I + 1, Count, N)
   end.

start() -> 
   StartList=lists:seq(10000, 10001),
   MapList=lists:map(fun(X) -> (for(1,0,X)) end, StartList),
   io:fwrite(integer_to_list(lists:last(MapList))),
   io:fwrite("~n").
