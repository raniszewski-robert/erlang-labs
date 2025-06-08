defmodule Pollutiondb.Reading do
  require Ecto.Query
  use Ecto.Schema

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add(station, date, time, type, value) do
    {year, month, day} = date
    {hour, min, sec} = time

    s_date = Date.new!(year, month, day)
    s_time = Time.new!(hour, min, sec)

    %Pollutiondb.Reading{}
    |> Ecto.Changeset.cast(%{
      date: s_date,
      time: s_time,
      type: type,
      value: value
    }, [:date, :time, :type, :value])
    |> Ecto.Changeset.put_assoc(:station, station)
    |> Ecto.Changeset.validate_required([:date, :time, :type, :value, :station])
    |> Pollutiondb.Repo.insert()
  end

  def add_now(station, type, value) do
    add(station, Date.utc_today(), Time.utc_now(), type, value)
  end

  def find_by_date(date) do
    Pollutiondb.Repo.all(
      Ecto.Query.where(Pollutiondb.Reading, date: ^date)
    )
    |> Pollutiondb.Repo.preload(:station)
  end
end
