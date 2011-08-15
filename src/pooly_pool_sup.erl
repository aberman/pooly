%%% -------------------------------------------------------------------
%%% Author  : Andrew Berman
%%% Description :
%%%
%%% Created : Jul 29, 2011
%%% -------------------------------------------------------------------
-module(pooly_pool_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).

-include("record.hrl").

start_link(Name, #config{} = Config) ->
    supervisor:start_link(?MODULE, [Name, Config]).

init([Name, #config{} = Config]) ->
    Pooly = {pooly, {pooly, start_link, [Name, Config]},
            permanent, 5000, worker, [pooly]},
    PoolSup = {pooly_member_sup, {pooly_member_sup, start_link, [Name]},
                permanent, 5000, supervisor, [pooly_member_sup]},
    {ok, {{one_for_all, 5, 10}, [PoolSup, Pooly]}}.