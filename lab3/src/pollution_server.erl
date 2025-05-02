-module(pollution_server).
-export([get_station/1, add_station/2, add_value/4,
  remove_value/3, get_one_value/3, get_station_min/2,
  get_daily_mean/2, get_area_mean/3, start/0, stop/0]).

start() ->
  register(poll_server, spawn(fun() -> loop(empty) end)),
  poll_server ! init.

stop() -> poll_server ! kill.

error_message(Message) ->
  io:format("Error message: ~s~n", [Message]).

loop(Monitor) ->
  receive
    kill -> ok;
    init ->
      loop(pollution:create_monitor());
    {Operation, Params, PID} ->
      NewList = tuple_to_list(Params) ++ [Monitor],
      Result = apply(pollution, Operation, NewList),
      case Result of
        {error, ErrorMessage} ->
          PID ! error_message(ErrorMessage),
          loop(Monitor);
        NewResult ->
          PID ! NewResult,
          case string:prefix(atom_to_list(Operation), "get") of
            nomatch -> loop(NewResult);
            _ -> loop(Monitor)
          end
      end;
    _ ->
      io:format("Nieznana funkcja"),
      loop(Monitor)
  end.

get_station(StationInfo) ->
  poll_server ! {get_station, {StationInfo}, self()},
  receive X -> X end.

add_station(Name, Coords) ->
  poll_server ! {add_station, {Name, Coords}, self()},
  receive X -> X end.

add_value(StationInfo, Date, Type, Value) ->
  poll_server ! {add_value, {StationInfo, Date, Type, Value}, self()},
  receive X -> X end.

remove_value(StationInfo, Date, Type) ->
  poll_server ! {remove_value, {StationInfo, Date, Type}, self()},
  receive X -> X end.

get_one_value(StationInfo, Date, Type) ->
  poll_server ! {get_one_value, {StationInfo, Date, Type}, self()},
  receive X -> X end.

get_station_min(StationInfo, Type) ->
  poll_server ! {get_station_min, {StationInfo, Type}, self()},
  receive X -> X end.

get_daily_mean(Type, Day) ->
  poll_server ! {get_daily_mean, {Type, Day}, self()},
  receive X -> X end.

get_area_mean(Type, Coords, Radius) ->
  poll_server ! {get_area_mean, {Type, Coords, Radius}, self()},
  receive X -> X end.