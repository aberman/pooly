%%% -------------------------------------------------------------------
%%% Author  : Andrew Berman
%%% Description :
%%%
%%% Created : Aug 4, 2011
%%% -------------------------------------------------------------------
-module(pooly_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
		 start_link/0
		]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
 	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Restart = {simple_one_for_one, 1, 1},
	Child = {pooly_pool_sup, {pooly_pool_sup, start_link, []},
				   permanent, 5000, supervisor, [pooly_pool_sup]},	
	{ok, {Restart, [Child]}}.