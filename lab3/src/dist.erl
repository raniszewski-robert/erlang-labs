
-module(dist).
-export([rand_locs/1, find_closest_parallel/2, find_closest/2, find_for_personPID/3]).

rand_locs(N) -> [{rand:uniform(10000), rand:uniform(10000)} ||_ <- lists:seq(1, N)].

dist({X1, Y1}, {X2, Y2}) -> math:sqrt((X1-X2)*(X1-X2) + (Y1 - Y2)*(Y1-Y2)).

find_for_person(PersonLoc, SensorsLoc) ->
  lists:min([{dist(PersonLoc, SL), {PersonLoc, SL}} || SL <- SensorsLoc]).

find_closest(PeopleLoc, SensorLoc) ->
  lists:min([find_for_person(PL, SensorLoc) || PL <- PeopleLoc]).

find_for_personPID(PersonLoc, SensorsLoc, PID) ->
  PID ! lists:min([{dist(PersonLoc, SL), {PersonLoc, SL}} || SL <- SensorsLoc]).

find_closest_parallel(PeopleLoc, SensorsLoc) ->
  _L1 = [spawn(?MODULE, find_for_personPID, [PL, SensorsLoc, self()]) || PL <- PeopleLoc],
  lists:min([receive Msg -> Msg end || _ <- PeopleLoc]).
