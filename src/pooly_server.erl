-module(pooly_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
		 start_link/2,
		 start_link/3,
		 check_in/1,
		 check_out/0,
		 close/1,
		 size/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ACQUIRE_INCREMENT, 3).
-define(INITIAL_POOL_SIZE, 3).
-define(MAX_POOL_SIZE, 15).
-define(MAX_IDLE_TIME, 0).
-define(MIN_POOL_SIZE, 3).
-define(MAX_CONNECTION_AGE, 0).

-record(state, {host, port, options, q, q_len = 0, total = 0}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    start_link("127.0.0.1", 8087).
start_link(Host, Port) ->
	start_link(Host, Port, []).
start_link(Host, Port, Options) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port, Options], []).

size() ->
	gen_server:call(?SERVER, size).
	
check_out() ->
	case gen_server:call(?SERVER, check_out) of
		{ok, _} = Reply -> 
			gen_server:cast(?SERVER, check_queue),
			Reply;
		Reply -> Reply
	end.

check_in(Pid) ->
	gen_server:cast(?SERVER, {check_in, Pid}).

close(Pid) ->
	gen_server:cast(?SERVER, {close, Pid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Host, Port, Options]) ->
	State = #state{host = Host, port = Port, options = Options},	
	PidList = new_connection(Host, Port, Options, ?INITIAL_POOL_SIZE),
	{ok, State#state{q = queue:from_list(PidList), q_len = ?INITIAL_POOL_SIZE, total = ?INITIAL_POOL_SIZE}}.	    

handle_call(size, _From, State) ->
	{reply, queue:len(State#state.q), State};
handle_call(check_out, _From, State) ->
	case queue:out(State#state.q) of
		{empty, _} -> {reply, {error, pool_reached_capacity}, State};
		{{value, Pid}, Q2} -> {reply, pooly_member:activate(Pid), State#state{q = Q2, 
															 q_len = State#state.q_len - 1}}
	end;
	
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(check_queue, State) ->
	Len = State#state.q_len,
	Total = State#state.total,
	Diff = ?MAX_POOL_SIZE - Total,
	Increment = if 
					Diff >= ?ACQUIRE_INCREMENT -> ?ACQUIRE_INCREMENT;
					Diff < ?ACQUIRE_INCREMENT andalso Diff > 0 -> Diff;
					true -> 0
				end,
	case Len < ?MIN_POOL_SIZE andalso Increment > 0 of
		true -> {noreply, State#state{q = queue:join(State#state.q, queue:from_list(new_connection(State#state.host, State#state.port, State#state.options, ?ACQUIRE_INCREMENT))),
									  q_len = Len + ?ACQUIRE_INCREMENT,
									  total = Total + ?ACQUIRE_INCREMENT}};
		false -> {noreply, State}
	end;
handle_cast({check_in, Pid} , State) ->
	{noreply, State#state{q = queue:in(Pid, State#state.q), q_len = State#state.q_len + 1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
new_connection(Host, Port, Options, Count) ->
	new_connection(Host, Port, Options, Count, []).

new_connection(_Host, _Port, _Options, 0, Acc) ->
	Acc;
new_connection(Host, Port, Options, Count, Acc) ->
    case supervisor:start_child(pooly_member_sup, [Host, Port]) of
        {ok, Pid} when is_pid(Pid) -> new_connection(Host, Port, Options, Count - 1, [Pid | Acc]);
        {ok, Pid, _} when is_pid(Pid) -> new_connection(Host, Port, Options, Count - 1, [Pid | Acc]);
        E -> exit(E)
    end.
