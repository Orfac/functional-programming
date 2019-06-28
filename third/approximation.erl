-module(approximation).
-export([approximate/2]).
-import(point, [x/1, y/1]).

approximate(Points, X) -> 
    [Last | First] = Points,
    X1 = x(Last),
    X2 = x(First),
    Y1 = y(Last),
    Y2 = y(First),
    (Y2 - Y1) * (X - X1) / (X2 - X1) - Y1.
        
