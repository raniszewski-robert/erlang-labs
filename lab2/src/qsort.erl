
-module(qsort).

-export([less_than/2, grt_eq_than/2, qs/1, random_elems/3, compare_speeds/3, fun1/0, fun2/0, calculate_mean/2]).

less_than(List, Arg) ->
  [X || X <- List, X < Arg].

grt_eq_than(List, Arg) ->
  [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).

random_elems(N,Min,Max)->
  [rand:uniform(Max - Min) + Min || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  {Time1, Time2}.
%%  io:format("Fun1: ~n", Time1),
%%  io:format("Fun2: ~n", Time2).


fun1()->
  F1 = fun
         ($o) -> $a;
         ($e) -> $o;
         (C) -> C
       end,
  lists:map(F1, "asdsaaaoooeee").

fun2() ->
  L1 = lists:filter(fun (X) -> X rem 3 == 0 end, [4,3,5,6,7,18,17,31,30,33]),
  length(L1).


calculate_mean(Readings, Type)->
  L1 = lists:map(fun ([_,_,Dane]) -> Dane end, Readings),
  L2 = lists:foldl(fun (A, B) -> A++B end, [], L1),
  L3 = lists:filter(fun({T, _}) -> T =:= Type end, L2),
  L4 = lists:map(fun ({_, Val}) -> Val end, L3),
  lists:sum(L4) / length(L4).

