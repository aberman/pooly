%%% -------------------------------------------------------------------
%%% Author  : Andrew Berman
%%% Description :
%%%
%%% Created : Aug 4, 2011
%%% -------------------------------------------------------------------
-module(pooly_member).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/4, 
         activate/1,
         deactivate/1
        ]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         init/1,
         terminate/3]).

-export([
         active/2,		
         idle/3,
         idle/2
        ]).

-record(state, {name, pid, timer, idle_timeout, max_age}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, MA, IdleTimeout, MaxAge) ->	
    gen_fsm:start_link(?MODULE, [Name, MA, IdleTimeout, MaxAge], []).

activate(Pid) ->
    gen_fsm:sync_send_event(Pid, activate).

deactivate(Pid) ->
    gen_fsm:send_event(Pid, deactivate).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Name, {M, A}, IdleTimeout, MaxAge]) ->    
    {ok, Pid} = erlang:apply(M, start_link, A),        
    erlang:link(Pid),
    process_flag(trap_exit, true),
    
    case MaxAge > 0 andalso MaxAge =/= infinity of
        false ->
            {ok, idle, #state{name = Name,
                              pid = Pid, 
                              idle_timeout = IdleTimeout, 
                              max_age = MaxAge}, IdleTimeout};        
        true ->
            {ok, idle, #state{name = Name,
                              pid = Pid, 
                              idle_timeout = IdleTimeout, 
                              max_age = MaxAge, 
                              timer = erlang:send_after(MaxAge, self(), expired)}, IdleTimeout}
    end.

idle(activate, _From, State) ->
    {reply, {ok, State#state.pid}, active, State};
idle(_Event, _From, State) ->
    {next_state, idle, State, State#state.idle_timeout}.

idle(timeout, State) ->
%%     case State#state.timer of
%%         undefined -> ok;
%%         Timer -> erlang:cancel_timer(Timer)
%%     end,
    gen_fsm:send_all_state_event(State#state.name, {member_timed_out, self()}),
    {next_state, idle, State, State#state.idle_timeout};
idle(_Event, State) ->
    {next_state, idle, State}.

active(deactivate, State) ->
    {next_state, idle, State, State#state.idle_timeout};
active(_Event, State) ->
    {next_state, active, State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, State_Name, State) ->
    {next_state, State_Name, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info({'EXIT', _Pid, _Reason}, _StateName, State) ->     
    gen_fsm:send_all_state_event(State#state.name, {member_exited, self()}),
    {stop, normal, State};
handle_info(expired, StateName, State) ->    
    gen_fsm:send_all_state_event(State#state.name, {member_expired, self()}),
    {next_state, StateName, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->    
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

