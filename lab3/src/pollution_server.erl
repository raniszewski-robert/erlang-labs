-module(pollution_server).
-export([get_station/1, add_station/2, add_value/4,
  remove_value/3, get_one_value/3, get_station_min/2,
  get_daily_mean/2, get_area_mean/3, start/0, stop/0]).


start() ->
  register(poll_server, spawn(fun() -> loop(empty) end)),
  poll_server ! init.

stop() -> poll_server ! kill.

error_message(Message) ->
  io:format("Error message: ~w~n", [Message]).

do_operation(get_station, StationInfo, Monitor) ->
pollution:get_station(StationInfo, Monitor);
do_operation(add_station, {Name, Coords}, Monitor) ->
pollution:add_station(Name, Coords, Monitor).

operation_handler(Operation, Params, Monitor) ->
  case do_operation(Operation, Params, Monitor) of
    {error, ErrorMessage} ->
      error_message(ErrorMessage),
      loop(Monitor);
    NewMonitor ->
      loop(NewMonitor)
  end.

loop(Monitor) ->
  receive
    kill -> ok;
    init ->
      loop(pollution:create_monitor());
    {Operation, Params} ->
      operation_handler(Operation, Params, Monitor);
    _ -> io:format("Nieznana funkcja")
  end.

get_station(StationInfo) ->
  poll_server ! {get_station, StationInfo}.

add_station(Name, Coords) ->
  poll_server ! {add_station, {Name, Coords}}.

add_value(StationInfo, Date, Type, Value) ->
  poll_server ! {add_value, {StationInfo, Date, Type, Value}}.

remove_value(StationInfo, Date, Type) ->
  poll_server ! {remove_value, {StationInfo, Date, Type}}.

get_one_value(StationInfo, Date, Type) ->
  poll_server ! {get_one_value, {StationInfo, Date, Type}}.

get_station_min(StationInfo, Type) ->
  poll_server ! {get_station_min, {StationInfo, Type}}.

get_daily_mean(Type, Day) ->
  poll_server ! {get_daily_mean, {Type, Day}}.


get_area_mean(Type, Coords, Radius) ->
  poll_server ! {get_area_mean, {Type, Coords, Radius}}.