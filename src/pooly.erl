%%% -------------------------------------------------------------------
%%% Author  : Andrew Berman
%%% Description :
%%%
%%% Created : Aug 4, 2011
%%% -------------------------------------------------------------------
-module(pooly).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(SERVER, ?MODULE).
%% --------------------------------------------------------------------
%% External exports
-export([         
         start_link/2,		 
         check_in/2,
         check_out/1,
         size/1,
         total/1
        ]).

%% gen_fsm callbacks
-export([
         init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4,
         busy/2,
         busy/3,
         exhausted/2,
         exhausted/3,
         ready/2, 
         ready/3
        ]).

-include("record.hrl").
-record(state, {ma, q, q_len = 0, total = 0, out = [], expired = [], config, name, sup_name}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Name, #config{} = Config) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [Config, Name], []).

size(Name) ->
    gen_fsm:sync_send_all_state_event(Name, size).

total(Name) ->
    gen_fsm:sync_send_all_state_event(Name, total).

check_out(Name) ->
    Reply = gen_fsm:sync_send_event(Name, check_out),
    gen_fsm:send_event(Name, update),
    Reply.

check_in(Name, Pid) ->
    gen_fsm:send_event(Name, {check_in, Pid}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([#config{} = Config, Name]) ->    
    NameStr = case is_atom(Name) of 
        true -> atom_to_list(Name);
        false -> Name
    end,
    InitialPoolSize = Config#config.initial_pool_size,
    State = #state{config = Config,					   
                   q_len = InitialPoolSize,
                   total = InitialPoolSize,
                   name = Name,
                   sup_name = list_to_existing_atom(NameStr ++ "_sup")},
    {ok, ready, State#state{q = queue:from_list(new_connection(State, InitialPoolSize))}}.

ready({check_in, Pid}, State) ->	
    {next_state, ready, internal_check_in(Pid, State)};
ready(_Event, State) ->
    {next_state, ready, State}.

ready(check_out, _From, State) ->		
    % Q should never be empty if pooly is ready
    {{value, Pid}, Q2} = queue:out(State#state.q),	
    NewQLen = State#state.q_len - 1,
    {ok, CPid} = pooly_member:activate(Pid),    
    {reply, {ok, CPid} , busy, State#state{q = Q2, q_len = NewQLen, out = [{CPid, Pid} |State#state.out]}};
ready(_Event, _From, State) ->
    {next_state, ready, State}.

busy(update, State) ->
    update_state(State);
%% Should we add a timeout here? We don't want it to remain busy forever.
busy(_Event, State) ->
    {next_state, busy, State}.

busy(_Event, _From, State) ->
    {reply, {error, busy}, busy, State}.

exhausted({check_in, Pid}, State) ->
    busy(update, internal_check_in(Pid, State));    
exhausted(_Event, State) ->
    {next_state, exhausted, State}.

exhausted(check_out, _From, State) ->
    {reply, {error, pool_exhausted}, exhausted, State};
exhausted(_Event, _From, State) ->
    {next_state, pool_exhausted, State}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event({member_exited, Pid}, _StateName, State) ->
    update_state(remove_pid(Pid, State));    
handle_event({member_expired, Pid}, StateName, State) ->
    case queue:member(Pid, State#state.q) of
        true -> update_state(remove_pid(Pid, State));
        false -> {next_state, StateName, State#state{expired = [Pid | State#state.expired]}}
    end;   
handle_event({member_timed_out, Pid}, _StateName, State) ->
    try
        Config = State#state.config,
        State#state.q_len > Config#config.min_pool_size orelse throw(ready),
        update_state(remove_pid(Pid, State))
    catch
        throw:ready -> {next_state, ready, State}
    end;
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(size, _From, StateName, State) ->
    {reply, {ok, State#state.q_len}, StateName, State};
handle_sync_event(total, _From, StateName, State) ->
    {reply, {ok, State#state.total}, StateName, State};
handle_sync_event(_Event, _From, StateName, StateData) ->   
    {reply, ok, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
new_connection(#state{} = State, Count) ->
    new_connection(State, Count, []).

new_connection(#state{} = _State, 0, Acc) ->
    Acc;
new_connection(#state{} = State, Count, Acc) ->
    Config = State#state.config,    
    case supervisor:start_child(State#state.sup_name, [State#state.name, {Config#config.module, Config#config.args}, 
                                                   Config#config.idle_timeout, 
                                                   Config#config.max_age]) of
        {ok, Pid} when is_pid(Pid) -> new_connection(State, Count - 1, [Pid | Acc]);
        {ok, Pid, _} when is_pid(Pid) -> new_connection(State, Count - 1, [Pid | Acc]);
        E -> exit(E)
    end.

internal_check_in(CPid, #state{} = State) ->
    Q = State#state.q,
    QLen = State#state.q_len,
    case lists:keytake(CPid, 1, State#state.out) of
        {value, {_, Pid}, Out} ->
            case lists:member(Pid, State#state.expired) of
                true -> 
                    remove_pid(Pid, State#state{expired = lists:delete(Pid, State#state.expired), 
                                                out = Out});
                false ->
                    pooly_member:deactivate(Pid),
                    State#state{q = queue:in(Pid, Q), q_len = QLen + 1, out = Out}
            end;
        false -> State
    end.

remove_pid(Pid, State) ->
    case supervisor:terminate_child(State#state.sup_name, Pid) of
        ok -> ok;
        {error, not_found} -> ok
    end,
    State2 = case queue:member(Pid, State#state.q) of
                 true ->
                     State#state{q = queue:from_list(lists:delete(Pid, queue:to_list(State#state.q))),
                                 q_len = State#state.q_len - 1};
                 false ->
                     State
             end,
    
    State2#state{total = State2#state.total - 1}.

update_state(#state{} = State) ->
    Config = State#state.config,
    try
        State#state.q_len < Config#config.min_pool_size orelse throw(ready),
        
        MaxPoolSize = Config#config.max_pool_size,
        Total = State#state.total,
        Diff = MaxPoolSize - Total,             
        Diff + State#state.q_len > 0 orelse throw(exhausted),
        
        Increment = erlang:min(Config#config.acquire_increment, Diff),      
        {next_state, ready, State#state{q = queue:join(State#state.q,
                                                       queue:from_list(new_connection(State, Increment))),
                                        q_len = State#state.q_len + Increment,
                                        total = Total + Increment}}     
    catch
        throw:exhausted -> {next_state, exhausted, State};
        throw:ready -> {next_state, ready, State}
    end.