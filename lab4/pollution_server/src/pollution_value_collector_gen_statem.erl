%%%-------------------------------------------------------------------
%%% @author Robert
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. maj 2025 18:51
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("Robert").

-record(state, {monitor, station=none, new_results=[]}).
-record(result, {type, date, value}).

-behaviour(gen_statem).

%% API
-export([start_link/0]).
-export([no_station/3, station_set/3]).
%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0]).
-export([set_station/1, add_value/3, store_data/0]).
-define(SERVER, ?MODULE).


start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, pollution:create_monitor(), []).

init(State) ->
    {ok, no_station, #state{monitor = State}}.

set_station(StationInfo) ->
    gen_statem:cast(?MODULE, {set_station, StationInfo}).

add_value(Value, Date, Type) ->
    gen_statem:cast(?MODULE, {add_value, Value, Date, Type}).

store_data()->
    gen_statem:cast(?MODULE, {store_data}).

no_station(cast, {set_station, StationInfo}, State) ->
    Station = pollution:get_station(StationInfo, State#state.monitor),
    case Station of
        {error, _} ->
            {keep_state_and_data};
        _ ->
            {next_state, station_set, State#state{station = Station}}
    end.

station_set(cast, {add_value, Value, Date, Type}, State) ->
    NewResult = #result{type = Type, date = Date, value = Value},
    NewResults = [NewResult | State#state.new_results],
    {keep_state, State#state{new_results = NewResults}};
station_set(cast, {store_data}, State) ->
    {StationName, _} = State#state.station,
    NewMonitor = add_data(State#state.new_results, StationName, State#state.monitor),
    {next_state, no_station, #state{monitor = NewMonitor}}.

add_data([], _StationInfo, Monitor) -> Monitor;
add_data([H | T], StationInfo, Monitor) ->
    NewMonitor = pollution:add_value(StationInfo, H#result.date, H#result.type, H#result.value, Monitor),
    case NewMonitor of
        {error, _} -> add_data(T, StationInfo, Monitor);
        _ -> add_data(T, StationInfo, NewMonitor)
    end.


terminate(_Reason, _StateName, _State) ->
    ok.

callback_mode() ->
    state_functions.
