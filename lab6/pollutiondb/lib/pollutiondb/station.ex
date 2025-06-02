defmodule Pollutiondb.Station do
  use Ecto.Schema
  require Ecto.Query

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float
  end

  def add(station) do
    Pollutiondb.Repo.insert(station)
  end
  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Station)
  end

  def get_by_id(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
  end

  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end

  def find_by_name(name) do
    Pollutiondb.Repo.all(
      Ecto.Query.where(Pollutiondb.Station, name: ^name) )
  end

  def find_by_location(lon, lat) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat)
    |> Pollutiondb.Repo.all
  end
  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: ^lon_min < s.lon,
      where: ^lon_max > s.lon,
      where: ^lat_min < s.lat,
      where: ^lat_max > s.lat
    )
    |> Pollutiondb.Repo.all
  end

  def update_name(station, newname) do
    Ecto.Changeset.cast(station, %{name: newname}, [:name])
    |> Ecto.Changeset.validate_required([:name])
    |> Pollutiondb.Repo.update
  end

  def add(name, lon, lat) do
    %{name: "", lon: 0, lat: 0}
    |> Ecto.Changeset.cast(%{name: name, lon: lon, lat: lat}, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name, :lon, :lat])
    |> Ecto.Changeset.validate_number(:lon, greater_than: 0, less_than: 1000)
    |> Ecto.Changeset.validate_number(:lat, greater_than: 0, less_than: 1000)
    |> Pollutiondb.Repo.insert
  end
end