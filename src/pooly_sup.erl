%%% -------------------------------------------------------------------
%%% Author  : Andrew Berman
%%% Description :
%%%
%%% Created : Aug 4, 2011
%%% -------------------------------------------------------------------
-module(pooly_sup).

-behaviour(application).
-behaviour(supervisor).

-define(DEFAULT_ACQUIRE_INCREMENT, 3).
-define(DEFAULT_INITIAL_POOL_SIZE, 5).
-define(DEFAULT_MAX_POOL_SIZE, infinity).
-define(DEFAULT_MIN_POOL_SIZE, 3).
-define(DEFAULT_IDLE_TIMEOUT, 2 * 60 * 60 * 1000).
-define(DEFAULT_MAX_AGE, infinity).

-define(CONFIG_KEYS, [acquire_increment, initial_pool_size, max_pool_size, min_pool_size, idle_timeout, max_age]).
-define(POOL_WORKER_KEYS, [module, args]).

-include("record.hrl").


%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([start_link/1, init/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    ConfigFile = case application:get_env(config_file) of
                     undefined -> exit({config_not_set, "Set the config_file application parameter"});
                     {ok, Val} -> Val
                 end,    
     pooly_sup:start_link(ConfigFile).    

stop(_State) ->    
    ok.

start_link(ConfigFile) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ConfigFile]).

init([ConfigFile]) ->
    {ok, Conf} = file:consult(ConfigFile),    
    PoolProps = lists:filter(fun({Key, _}) -> not lists:member(Key, ?CONFIG_KEYS) end, Conf),
    % Get Any Default Settings

    DefaultConfig = #config{
                            acquire_increment = proplists:get_value(acquire_increment, Conf, ?DEFAULT_ACQUIRE_INCREMENT),
                            initial_pool_size = proplists:get_value(initial_pool_size, Conf, ?DEFAULT_INITIAL_POOL_SIZE),
                            max_pool_size = proplists:get_value(max_pool_size, Conf, ?DEFAULT_MAX_POOL_SIZE),
                            min_pool_size = proplists:get_value(min_pool_size, Conf, ?DEFAULT_MIN_POOL_SIZE),
                            idle_timeout = proplists:get_value(idle_timeout, Conf, ?DEFAULT_IDLE_TIMEOUT),
                            max_age = proplists:get_value(max_age, Conf, ?DEFAULT_MAX_AGE)
                           },
    Pools = lists:map(
              fun({Name, Props}) ->           
                      Mod = proplists:get_value(module, Props),
                      Mod =/= undefined orelse exit({error, bad_pooly_config}),
                      {Name, {pooly_pool_sup, start_link, [Name, DefaultConfig#config{                              
                                                   module = Mod,
                                                   args = proplists:get_value(args, Props, []),
                                                   acquire_increment = proplists:get_value(acquire_increment, Props, DefaultConfig#config.acquire_increment),
                                                   initial_pool_size = proplists:get_value(initial_pool_size, Props, DefaultConfig#config.initial_pool_size),
                                                   max_pool_size = proplists:get_value(max_pool_size, Props, DefaultConfig#config.max_pool_size),
                                                   min_pool_size = proplists:get_value(min_pool_size, Props, DefaultConfig#config.min_pool_size),
                                                   idle_timeout = proplists:get_value(idle_timeout, Props, DefaultConfig#config.idle_timeout),
                                                   max_age = proplists:get_value(max_age, Props, DefaultConfig#config.max_age)
                                                  }]}, permanent, 5000, supervisor, [pooly_pool_sup]}                                                
              end                                                                  
              , PoolProps),
    
    Restart = {one_for_one, 10, 10},
    {ok, {Restart, Pools}}.
