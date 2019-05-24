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
      if (UpdatedCount =:= N) -> 1;
      true -> 1 + getPrime(I + 1, UpdatedCount, N)
      end;
   false -> 1 + getPrime(I + 1, Count, N)
   end.

getPrime(N) -> getPrime(1, 0, N).

start() -> 
   io:fwrite(integer_to_list(getPrime(10001))),
   io:fwrite("~n").
