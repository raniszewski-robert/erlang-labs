
-module(pollutionCalc).

-export([testData/0, number_of_readings/2, calculate_max/2, calculate_min/2]).

testData() ->
  P1 = ["P1", {date(), time()}, [{pm10, 42.1}, {pm25, 35.6}, {temperature, 21.4}, {pressure, 1013}]],
  P2 = ["P2", {{2025, 3, 23}, {8, 30, 0}}, [{pm25, 37.0}, {temperature, 17.8}, {humidity, 72}]],
  P3 = ["P3", {{2025, 3, 13}, {18, 28, 24}}, [{pm10, 58.0}, {pm25, 41.3}, {pm1, 20.5}, {temperature, 19.1}, {pressure, 1015}, {humidity, 60}]],
  [P1, P2, P3].

number_of_readings([], _) -> 0;
number_of_readings(Readings, Date) ->
  [H|T] = Readings,
  [_,{ReadDate,_}, Pomiar] = H,
  case ReadDate of
    Date -> length(Pomiar) + number_of_readings(T, Date);
    _ -> number_of_readings(T, Date)
  end.

calculate_max(Readings, Type)  ->
  Values = [Value || [_, _, Pomiar] <- Readings, {ValueType, Value} <- Pomiar, ValueType =:= Type],
  case Values of
    [] -> undefined;
    _ -> lists:max(Values)
  end.

calculate_min(Readings, Type)  ->
  Values = [Value || [_, _, Pomiar] <- Readings, {ValueType, Value} <- Pomiar, ValueType =:= Type],
  case Values of
    [] -> undefined;
    _ -> lists:min(Values)
  end.
