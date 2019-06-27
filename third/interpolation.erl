-module(interpolation).
-export([interpolate/2, create_point/2]).

-include("point.hrl").
interpolate(Points, X) -> 
    lists:foldl(
        fun (Point, Acc) -> 
            Acc + y(Point) * base_polynomial(Points,X,Point)
        end,
        0, Points).

base_polynomial(Points, X, NextPoint) ->
    UpdatedList = lists:delete(NextPoint, Points),
    lists:foldl(
        fun(Point, Acc) -> 
            Acc * (X - x(Point)) / (x(NextPoint) - x(Point) )
        end, 1, UpdatedList).
        

x(Point) -> Point#point.x.
y(Point) -> Point#point.y.
create_point(X,Y) -> #point{x = X, y = Y}.