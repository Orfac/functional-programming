-module(point). 
-export([x/1, y/1, create_point/2, str_to_point/1, str_to_ordinate/1]).

-record(point, {x, y}).


x(Point) -> Point#point.x.
y(Point) -> Point#point.y.
create_point(X,Y) -> #point{x = X, y = Y}.

str_to_float(Str) ->
    try float(list_to_integer(Str))
    catch error:_ -> list_to_float(Str)
    end. 

str_to_point(Str) ->
   Point_Tokens = string:tokens(Str, ";"),
   [ X_String | Y_String ] = Point_Tokens,
   X = str_to_float(X_String),
   Y = str_to_float(string:trim(Y_String)),
   create_point(X,Y).

str_to_ordinate(Str) ->
    str_to_float(string:trim(Str)).