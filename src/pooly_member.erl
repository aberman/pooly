-module(pooly_member).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/5, 
         get_pid/1
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

-record(state, {pid, timer, idle_timeout, max_age}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Host, Port, Options, IdleTimeout, MaxAge) ->	
    gen_fsm:start_link(?MODULE, [Host, Port, Options, IdleTimeout, MaxAge], []).

get_pid(Pid) ->
    gen_fsm:sync_send_event(Pid, activate).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Host, Port, Options, IdleTimeout, MaxAge]) ->
    process_flag(trap_exit, true),
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port, Options),        
    erlang:link(Pid),
    
    case MaxAge > 0 andalso MaxAge =/= infinity of
        false ->
            {ok, idle, #state{pid = Pid, 
                              idle_timeout = IdleTimeout, 
                              max_age = MaxAge}, IdleTimeout};        
        true ->
            {ok, idle, #state{pid = Pid, 
                              idle_timeout = IdleTimeout, 
                              max_age = MaxAge, 
                              timer = erlang:send_after(MaxAge, self(), expired)}, IdleTimeout}
    end.

idle(activate, _From, State) ->
    {reply, {ok, State#state.pid}, active, State};
idle(_Event, _From, State) ->
    {next_state, idle, State, State#state.idle_timeout}.

idle(timeout, State) -> 
    %% Send message that I timed out but don't necessarily quit, let pooly terminate me
    error_logger:info_msg("~s timed out", [pid_to_list(self())]),
    
    case State#state.timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    gen_fsm:send_all_state_event(pooly, {timed_out, self()}),
    {next_state, idle, State};
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

handle_info({'EXIT', Pid, _Reason}, _StateName, State) ->     
    error_logger:info_msg("~s exited", [pid_to_list(Pid)]),
    gen_fsm:send_all_state_event(pooly, {cleanup, self()}),
    {stop, normal, State};
handle_info(expired, StateName, State) ->
    error_logger:info_msg("~s expired", [self()]),
    gen_fsm:send_all_state_event(pooly, {expired, self()}),
    {next_state, StateName, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    riakc_pb_socket:stop(State#state.pid),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

