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
   StartList=lists:seq(1, for(1,0,10001)),
   FilteredList=lists:filter(fun(X) -> isPrime(X) end, StartList),
   FoldedValue=lists:foldl(fun(X, _) -> X end, 0, FilteredList),
   io:fwrite(integer_to_list(FoldedValue)),
   io:fwrite("~n").
