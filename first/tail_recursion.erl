-module(main).  
-export([for/3,isPrime/1,isPrime/3,start/0]). 

isPrime(1) -> false;
isPrime(2) -> true;
isPrime(N) -> isPrime(N, 2, N div 2).

isPrime(N, Divider, MaxDivider) when (Divider > MaxDivider) -> true;
isPrime(N, Divider, MaxDivider) ->
   if (N rem Divider == 0) -> false;
   true -> isPrime(N, Divider + 1, MaxDivider)
   end.

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
   io:fwrite(integer_to_list(for(1,0,10001))),
   io:fwrite("~n").


