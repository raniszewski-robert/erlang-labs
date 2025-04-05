
-module(myLists).

-export([contains/2, duplicateElements/1, sumFloats/1, sumFloats/2]).

contains([], _) -> false;
contains([E | _], E) -> true;
contains([_|T], E) -> contains(T, E).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).

sumFloats([]) -> 0.0;
sumFloats([H|T]) when is_float(H) -> H + sumFloats(T);
sumFloats([H|T]) when not is_float(H) -> sumFloats(T).


sumFloats([], S) -> S;
sumFloats([H|T], S) when is_float(H) -> sumFloats(T, S+H);
sumFloats([H|T], S) when not is_float(H) -> sumFloats(T, S).