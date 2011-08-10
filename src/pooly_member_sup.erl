%%% -------------------------------------------------------------------
%% Author  : andrew
%%% Description :
%%%
%%% Created : Jul 29, 2011
%%% -------------------------------------------------------------------
-module(pooly_member_sup).

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
	Child = {pooly_member, {pooly_member, start_link, []},
				   temporary, brutal_kill, worker, [pooly_member]},	
	{ok, {Restart, [Child]}}.