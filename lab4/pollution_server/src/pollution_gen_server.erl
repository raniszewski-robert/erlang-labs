%%%-------------------------------------------------------------------
%%% @author Robert
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-export([get_station/1, get_area_mean/3, get_daily_mean/2, get_station_min/2, get_one_value/3]).
-export([add_station/2, add_value/4, remove_value/3]).
-export([crash/0]).
-define(SERVER, ?MODULE). 

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start()->
    start_link(pollution:create_monitor()).

start_link(InitVal) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitVal, []).

stop() -> gen_server:cast(?MODULE, stop).

init(State) ->
    {ok, State}.

print_error(Error_message) ->
    io:format("ERROR: ~w. ~n", [Error_message]),
    ok.

get_station(StationInfo) -> gen_server:call(?MODULE, {get_station, {StationInfo}}).
get_one_value(StationInfo, Date, Type) ->
    gen_server:call(?MODULE, {get_one_value, {StationInfo, Date, Type}}).
get_station_min(StationInfo, Type) ->
    gen_server:call(?MODULE, {get_station_min, {StationInfo, Type}}).
get_daily_mean(Type, Day) ->
    gen_server:call(?MODULE, {get_daily_mean, {Type, Day}}).
get_area_mean(Type, Coords, Radius) ->
    gen_server:call(?MODULE, {get_area_mean, {Type, Coords, Radius}}).

add_station(Name, Coords) ->
    gen_server:cast(?MODULE, {add_station, {Name, Coords}}).
add_value(StationInfo, Date, Type, Value) ->
    gen_server:cast(?MODULE, {add_value, {StationInfo, Date, Type, Value}}).
remove_value(StationInfo, Date, Type) ->
    gen_server:cast(?MODULE, {remove_value, {StationInfo, Date, Type}}).


handle_call(Args, _From, Monitor) ->
    {Operation, FunArgs} = Args,
    NewArgsList = tuple_to_list(FunArgs) ++ [Monitor],
    Result = apply(pollution, Operation, NewArgsList),
    {reply, Result, Monitor}.


handle_cast(Args, Monitor) ->
    case Args of
        stop -> {stop, normal, Monitor};
        _ ->
            {Operation, FunArgs} = Args,
            NewArgsList = tuple_to_list(FunArgs) ++ [Monitor],
            Result = apply(pollution, Operation, NewArgsList),
            case Result of
                {error, ErrorMessage} ->
                    print_error(ErrorMessage),
                    {noreply, Monitor};
                _ -> {noreply, Result}
            end
    end.

terminate(Reason, State) ->
    io:format("ERROR: ~w. ~n", [Reason]),
    io:format("State: ~w. ~n", [State]),
    ok.

crash() -> 1/0.