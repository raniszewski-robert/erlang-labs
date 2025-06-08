defmodule PollutionDataLoader do
  def parse_csv(file_path) do
    file = File.read!("D:\\Profil\\Downloads\\pollution_50k.csv\\AirlyData-ALL-50k.csv")
           |> String.trim()
           |> String.split("\n")
           |> Enum.map(&parse_line/1)
  end
  def parse_line(line) do
    [date, type, value, id, name, coords] = line |> String.split(";")

    %{
      :date => date |> parse_date(),
      :type => type, :value => value |> String.to_float(),
      :id => id |> String.to_integer(),
      :name => name,
      :coords => coords |> String.split(",") |> Enum.map(&String.to_float/1) |> List.to_tuple()
    }
  end
  defp parse_date(date) do
    [date, time] = date |> String.split("T")
    date = date |> String.split("-")
           |> Enum.map(&String.to_integer/1) |> List.to_tuple()
    time_len = String.length(time)
    time = time |> String.slice(0, time_len - 5) |> String.split(":")
           |> Enum.map(&String.to_integer/1) |> List.to_tuple()
    {date, time}
  end

  def id_stations(stations) do
    stations |> Enum.uniq_by(& &1[:id])
  end

  defp measure_fun(data, fun) do
    time = :timer.tc(fn -> Enum.map(data, fun) end) |> elem(0)
    time / 1000000
  end

  def load(data_path) do
    data = parse_csv(data_path)
    uniq_stations = identifyStations(data)

    # Run pollution gen server
    Code.append_path("D:\\Profil\\Desktop\\Informatyka\\4sem\\erlang-labs\\lab4\\pollution_server\\_build\\default\\lib\\pollution_server\\ebin")

    case Application.start(:pollution_server) do
      :ok ->
        IO.puts("lab4 started successfully")
        # Add stations to the server
        time = measure_fun(uniq_stations, fn station ->
          id_name = "#{station[:id]} #{station[:name]}"
          :pollution_gen_server.add_station(id_name, station[:coords])
        end)
        IO.puts("added unique stations to the server [#{time}]s")
        # Add measurements to the server
        time = Timer.measure_fun(file, fn station ->
          :pollution_gen_server.add_value(station[:coords], station[:date], station[:type], station[:value])
        end)
        IO.puts("added measurements [#{time}]s")
      {:error, _} ->
        IO.puts("Failed to start :pollution_server")
    end
  end
end
