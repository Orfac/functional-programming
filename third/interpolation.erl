-module(interpolation).
-export([interpolate/2]).
-import(point, [x/1, y/1]).

interpolate(Points, X) -> 
    lists:foldl(
        fun (Point, Acc) -> 
            Acc + y(Point) * base_polynomial(Points,X,Point)
        end,
        float(0), Points).

base_polynomial(Points, X, NextPoint) ->
    UpdatedList = lists:delete(NextPoint, Points),
    lists:foldl(
        fun(Point, Acc) -> 
            Acc * ((X - x(Point)) / (x(NextPoint) - x(Point)))
        end, float(1), UpdatedList).
        
