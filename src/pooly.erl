%%% -------------------------------------------------------------------
%%% Author  : andrew
%%% Description :
%%%
%%% Created : Aug 4, 2011
%%% -------------------------------------------------------------------
-module(pooly).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(DEFAULT_ACQUIRE_INCREMENT, 3).
-define(DEFAULT_INITIAL_POOL_SIZE, 5).
-define(DEFAULT_MAX_POOL_SIZE, infinity).
-define(DEFAULT_MIN_POOL_SIZE, 3).
-define(DEFAULT_IDLE_TIMEOUT, 2 * 60 * 60 * 1000).
-define(DEFAULT_MAX_AGE, infinity).
-define(DEFAULT_PORT, 8087).
-define(DEFAULT_HOST, "127.0.0.1").

-define(SERVER, ?MODULE).
%% --------------------------------------------------------------------
%% External exports
-export([
         check_in/1,
         check_out/0,
         size/0,
         start_link/0,		 
         total/0
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

-record(state, {host, port, options, q, q_len = 0, total = 0, out = [], expired = [], acquire_increment, initial_pool_size, max_pool_size, min_pool_size, max_idle_time, max_age}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

size() ->
    gen_fsm:sync_send_all_state_event(?SERVER, size).

total() ->
    gen_fsm:sync_send_all_state_event(?SERVER, total).

check_out() ->
    Reply = gen_fsm:sync_send_event(?SERVER, check_out),
    gen_fsm:send_event(?SERVER, update),
    Reply.

check_in(Pid) ->
    gen_fsm:send_event(?SERVER, {check_in, Pid}),
    gen_fsm:send_event(?SERVER, update).

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
init([]) ->	
    {ok, Conf} = file:consult(filename:join(
                                [filename:dirname(code:which(?MODULE)),
                                 "..", "priv", "pooly.conf"])),
    InitialPoolSize = proplists:get_value(initial_pool_size, Conf, ?DEFAULT_INITIAL_POOL_SIZE),
    AcquireIncrement = proplists:get_value(acquire_increment, Conf, ?DEFAULT_ACQUIRE_INCREMENT),
    InitialPoolSize = proplists:get_value(initial_pool_size, Conf, ?DEFAULT_INITIAL_POOL_SIZE),
    MaxPoolSize = proplists:get_value(max_pool_size, Conf, ?DEFAULT_MAX_POOL_SIZE),
    MinPoolSize = proplists:get_value(min_pool_size, Conf, ?DEFAULT_MIN_POOL_SIZE),
    MaxIdleTime = proplists:get_value(idle_timeout, Conf, ?DEFAULT_IDLE_TIMEOUT),
    MaxAge = proplists:get_value(max_age, Conf, ?DEFAULT_MAX_AGE),
    Host = proplists:get_value(host, Conf, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Conf, ?DEFAULT_PORT),    
    Options = proplists:get_value(options, Conf, []),
    MinPoolSize < MaxPoolSize orelse exit("min_pool_size must be less than max_pool_size"),
    State = #state{host = Host, 
                   port = Port, 
                   options = Options,
                   acquire_increment = AcquireIncrement,
                   initial_pool_size = InitialPoolSize,
                   max_age = MaxAge,
                   max_idle_time = MaxIdleTime,
                   max_pool_size = MaxPoolSize,
                   min_pool_size = MinPoolSize,					   
                   q_len = InitialPoolSize,
                   total = InitialPoolSize},
    {ok, ready, State#state{q = queue:from_list(new_connection(State, InitialPoolSize))}}.

ready({check_in, Pid}, State) ->	
    {next_state, ready, check_in(Pid, State)};
ready(_Event, State) ->
    {next_state, ready, State}.

ready(check_out, _From, State) ->		
    % Q should never be empty if pooly is ready
    {{value, Pid}, Q2} = queue:out(State#state.q),	
    NewQLen = State#state.q_len - 1,
    {ok, CPid} = pooly_member:get_pid(Pid),    
    {reply, {ok, CPid} , busy, State#state{q = Q2, q_len = NewQLen, out = [{CPid, Pid} |State#state.out]}};
ready(_Event, _From, State) ->
    {next_state, ready, State}.

busy(update, State) ->
    try
        State#state.q_len < State#state.min_pool_size orelse throw(ready),
        
        MaxPoolSize = State#state.max_pool_size,
        Total = State#state.total,
        Diff = MaxPoolSize - Total,				
        Diff + State#state.q_len > 0 orelse throw(exhausted),
        
        Increment = erlang:min(State#state.acquire_increment, Diff),		
        {next_state, ready, State#state{q = queue:join(State#state.q,
                                                       queue:from_list(new_connection(State, Increment))),
                                        q_len = State#state.q_len + Increment,
                                        total = Total + Increment}}		
    catch
        throw:exhausted -> {next_state, exhausted, State};
        throw:ready -> {next_state, ready, State}
    end;
%% Should we add a timeout here? We don't want it to remain busy forever.
busy(_Event, State) ->
    {next_state, busy, State}.

busy(_Event, _From, State) ->
    {reply, {error, busy}, busy, State}.

exhausted({check_in, Pid}, State) ->	
    {next_state, busy, check_in(Pid, State)};
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
handle_event({cleanup, Pid}, _StateName, State) ->
    busy(update, State#state{q = queue:from_list(lists:delete(Pid, queue:to_list(State#state.q))), 
                             q_len = State#state.q_len - 1, 
                             total = State#state.total - 1});    
handle_event({expired, Pid}, StateName, State) ->
    case queue:member(Pid, State#state.q) of
        true -> handle_event({cleanup, Pid}, StateName, State);
        false -> {next_state, StateName, State#state{expired = [Pid | State#state.expired]}}
    end;   
handle_event({timed_out, Pid}, StateName, State) ->    
    handle_event({cleanup, Pid}, StateName, State);          

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
    case supervisor:start_child(pooly_member_sup, [State#state.host, 
                                                   State#state.port, 
                                                   State#state.options, 
                                                   State#state.max_idle_time, 
                                                   State#state.max_age]) of
        {ok, Pid} when is_pid(Pid) -> new_connection(State, Count - 1, [Pid | Acc]);
        {ok, Pid, _} when is_pid(Pid) -> new_connection(State, Count - 1, [Pid | Acc]);
        E -> exit(E)
    end.

check_in(CPid, #state{} = State) ->
    Q = State#state.q,
    QLen = State#state.q_len,
    case lists:keytake(CPid, 1, State#state.out) of
        {value, {_, Pid}, Out} ->
            case lists:member(Pid, State#state.expired) of
                true -> 
                    ok = supervisor:terminate_child(pooly_member_sup, Pid),
                    State#state{total = State#state.total - 1, 
                                expired = lists:delete(Pid, State#state.expired), 
                                out = Out};
                false ->
                    State#state{q = queue:in(Pid, Q), q_len = QLen + 1, out = Out}
            end;
        false -> State
    end.
