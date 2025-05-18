-module(pollution).
-export([create_monitor/0, add_station/3, add_value/5,
  remove_value/4, get_one_value/4, get_station_min/3,
  get_daily_mean/3, get_area_mean/4, get_station/2]).

-record(station, {name, coords, results}).
% {string, {number, number}, [#result, #result,...]}
-record(result, {type, date, value}).
% {string, {{int, int, int},{int, int, int}}, number}
-record(monitor, {nameMap, coordsMap}).
% {#{name => #station, name => #station, ...}, #{coords => #station, coords => station, ...}}


create_monitor() -> #monitor{nameMap = #{}, coordsMap = #{}}.

get_station(StationInfo, Monitor) ->
  case is_tuple(StationInfo) of
    true -> maps:get(StationInfo, Monitor#monitor.coordsMap, {error, "brak stacji o podanych parametrach"});
    false -> maps:get(StationInfo, Monitor#monitor.nameMap, {error, "brak stacji o podanych parametrach"})
  end.

add_station(Name, Coords, Monitor) ->
  case maps:is_key(Name, Monitor#monitor.nameMap) or maps:is_key(Coords, Monitor#monitor.coordsMap) of
    true ->
      {error, "Nieprawidlowa stacja"};
    false ->
      Station = #station{name = Name, coords = Coords, results = []},
      NameMap = Monitor#monitor.nameMap,
      CoordsMap = Monitor#monitor.coordsMap,
      Monitor#monitor{nameMap = NameMap#{Name => Station}, coordsMap = CoordsMap#{Coords => Station}}
  end.

add_value(StationInfo, Date, Type, Value, Monitor) ->
  Station = get_station(StationInfo, Monitor),
  case Station of
    {error, _} -> {error, "Stacja nie istnieje"};
    _ ->
      Result = #result{type = Type, date = Date, value = Value},
      ResultCheck = {Type, Date},
      Results = Station#station.results,
      case lists:member(ResultCheck, lists:map(fun (Res) -> {Res#result.type, Res#result.date} end, Results)) of
        true -> {error, "Istnieje już pomiar o tych parametrach"};
        false ->
          NewResults = [Result | Results],
          NewStation = Station#station{results = NewResults},
          NewCoordsMap = (Monitor#monitor.coordsMap)#{NewStation#station.coords := NewStation},
          NewNameMap = (Monitor#monitor.nameMap)#{NewStation#station.name := NewStation},
          Monitor#monitor{nameMap = NewNameMap, coordsMap = NewCoordsMap}
      end
  end.



remove_value(StationInfo, Date, Type, Monitor) ->
  Station = get_station(StationInfo, Monitor),

  case Station of
    {error, _} -> {error, "Stacja nie istnieje"};
    _ ->
      Results = Station#station.results,
      ResultsSize = length(Results),
      NewResults = lists:filter(fun(Res) -> not((Res#result.date == Date) and (Res#result.type == Type)) end, Results),
      NewResultsSize = length(NewResults),
      case (ResultsSize - NewResultsSize) == 1 of
        false -> {error, "Pomiar nie istnieje"};
        true ->
          NewStation = Station#station{results = NewResults},
          NewCoordsMap = (Monitor#monitor.coordsMap)#{NewStation#station.coords := NewStation},
          NewNameMap = (Monitor#monitor.nameMap)#{NewStation#station.name := NewStation},
          Monitor#monitor{nameMap = NewNameMap, coordsMap = NewCoordsMap}
      end
  end.


get_one_value(StationInfo, Date, Type, Monitor) ->
  Station = get_station(StationInfo, Monitor),
  case Station of
    {error, _} -> {error, "Stacja nie istnieje"};
    _ ->
      Results = Station#station.results,
      NewResults = lists:filter(fun(Res) -> (Res#result.date == Date) and (Res#result.type == Type) end, Results),
      NewResultsSize = length(NewResults),
      case NewResultsSize == 1 of
        false -> {error, "Pomiar nie istnieje"};
        true ->
          [Result|_] = NewResults,
          Result#result.value
      end
  end.

get_station_min(StationInfo, Type, Monitor) ->
  Station = get_station(StationInfo, Monitor),
  case Station of
    {error, _} -> {error, "Stacja nie istnieje"};
    _ ->
      Results = Station#station.results,
      NewResults = lists:filter(fun(Res) -> Res#result.type == Type end, Results),
      Values = lists:map(fun(Res) -> Res#result.value end, NewResults),
      case Values of
        [] -> {error, "Brak wartosci"};
        _ -> lists:min(Values)
      end
  end.

get_daily_mean(Type, Day, Monitor) ->
  StationMap = Monitor#monitor.nameMap,
  Results = maps:map(fun(_, Val) -> Val#station.results end, StationMap),
  Results1 = lists:map(fun({_, X}) -> X end, maps:to_list(Results)),
  Results2 = lists:foldl(fun (A, B) -> A++B end, [], Results1),
  ResultsType = lists:filter(fun(Res) ->
    Date = Res#result.date,
    {DayCheck, _} = Date,
    (DayCheck == Day) and (Res#result.type == Type) end, Results2),
  ValueList = lists:map(fun(Res) -> Res#result.value end, ResultsType),
  case ValueList of
    [] -> {error, "Brak wartosci"};
    _ -> lists:sum(ValueList) / length(ValueList)
  end.

is_in_area(StartCoords, EndCoords, Radius) ->
  {StartX, StartY} = StartCoords,
  {EndX, EndY} = EndCoords,
  math:sqrt(math:pow(StartX - EndX, 2) + math:pow(StartY - EndY, 2)) < Radius.

get_area_mean(Type, Coords, Radius, Monitor) ->
  StationMap = Monitor#monitor.nameMap,
  Results = maps:map(fun(_, Val) -> {Val#station.coords ,Val#station.results} end, StationMap),
  CoordsResList = lists:map(fun({_, X}) -> X end, maps:to_list(Results)),
  InAreaList = lists:filter(fun({X, _}) -> is_in_area(Coords, X, Radius) end, CoordsResList),
  AreaValues = lists:map(fun({_, X}) -> X end, InAreaList),
  Values = lists:foldl(fun (A, B) -> A++B end, [], AreaValues),
  ResultsType = lists:filter(fun(Res) -> Res#result.type == Type end, Values),
  ValueList = lists:map(fun(Res) -> Res#result.value end, ResultsType),
  case ValueList of
    [] -> {error, "Brak wartosci"};
    _ -> lists:sum(ValueList) / length(ValueList)
  end.