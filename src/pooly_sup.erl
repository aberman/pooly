%%% -------------------------------------------------------------------
%%% Author  : andrew
%%% Description :
%%%
%%% Created : Jul 29, 2011
%%% -------------------------------------------------------------------
-module(pooly_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%%     Config = application:get_all_env(),
    Pooly = {pooly, {pooly, start_link, []},
            permanent, 5000, worker, [pooly]},
    PoolSup = {pooly_member_sup, {pooly_member_sup, start_link, []},
                permanent, 5000, supervisor, [pooly_member_sup]},
    {ok, {{one_for_all, 5, 10}, [PoolSup, Pooly]}}.