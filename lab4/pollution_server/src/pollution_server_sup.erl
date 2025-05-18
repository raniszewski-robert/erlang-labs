%%%-------------------------------------------------------------------
%% @doc pollution_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 2,
        period => 3
    },
    ChildSpecs = [#{id => 'pollution_gen_server',
            start => {pollution_gen_server, start, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [pollution_gen_server]}],
    {ok, {SupFlags, ChildSpecs}}.
