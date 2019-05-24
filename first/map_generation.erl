-module(main).  
-export([getPrime/3,getPrime/1,isPrime/1,isPrime/3,start/0]). 

isPrime(1) -> false;
isPrime(2) -> true;
isPrime(N) -> isPrime(N, 2, N div 2).

isPrime(_, Divider, MaxDivider) when (Divider > MaxDivider) -> true;
isPrime(N, Divider, _) when (N rem Divider == 0) -> false;
isPrime(N, Divider, MaxDivider) -> isPrime(N, Divider + 1, MaxDivider).

getPrime(I,Count,N) -> 
   case (isPrime(I)) of
   true ->   
      UpdatedCount = Count + 1,
      if (UpdatedCount =:= N) -> I;
      true -> getPrime(I + 1, UpdatedCount, N)
      end;
   false -> getPrime(I + 1, Count, N)
   end.

getPrime(N) -> getPrime(1, 0, N).

start() -> 
   StartList=lists:seq(10000, 10001),
   MapList=lists:map(fun(X) -> (getPrime(X)) end, StartList),
   io:fwrite(integer_to_list(lists:last(MapList))),
   io:fwrite("~n").
