%%%-------------------------------------------------------------------
%%% @author caibaohua
%%% @copyright (C) 2013, <The NetCircle>
%%% @doc
%%%
%%% @end
%%% Created : 14. 十一月 2013 上午11:54
%%%-------------------------------------------------------------------
-module(mod_auth_memcache).
-author("caibaohua").

-behaviour(gen_server).

%% API
-export([start_link/2, check_password/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).


-include("ejabberd.hrl").
-include("jlib.hrl").

-define(RETRY_TIMEOUT, 30000). %% 30 seconds
-define(BACKOFF_INTERVAL, 30000).  %% 30 seconds
-define(MAX_RETRY_BACKOFF, 1800000). %% 30 minutes


-record(state, {
    num_servers,
    buckets :: dict(),
    num_buckets = 0,
    ejabberd_host,
    retry_timeout :: integer(),
    backoff_interval :: integer(),
    max_retry_backoff :: integer(),
    workers_per_conn :: integer()
}).

-record(mmc, {
    conn,
    host,
    port,
    weight,
    init_time,
    timeout,
    timeoutms, %% takes precedence over timeout
    connect_timeoutms, %% takes precedence over timeout
    retry_interval,
    persistent,
    status,
    error, %% last error message
    errnum %% last error code
}).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Opts], []).

check_password(Password) ->
    case gen_server:call(?MODULE, {get_script, Password}) of
        {ok, Mmc} ->
            case (catch memcached_conn:get(Mmc#mmc.conn, Password)) of
                {error, not_found} ->
                    ?INFO_MSG("not found in conn~n", []),
                    false;
                {error, Reason} ->
                    memcached_conn:stop(Mmc#mmc.conn),
                    ?INFO_MSG("some other error occurred: kill it with fire!~n~p~n", [Reason]),
                    false;
                {'EXIT', R} ->
                    ?ERROR_MSG("EXIT exception when getting: ~p", [R]),
                    false;
                Val ->
                    auth_utils:auth_check(Val)
    end;
        {error, notfound} ->
            ?ERROR_MSG("Memcache connection not found. Check status of memcached server", []),
            false
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Opts]) ->
    Hosts = proplists:get_value(hosts, Opts, []),
    RetryTimeout = proplists:get_value(retry_timeout, Opts, ?RETRY_TIMEOUT),
    BackoffInterval = proplists:get_value(backoff_interval, Opts, ?BACKOFF_INTERVAL),
    MaxBackoffInterval = proplists:get_value(max_retry_backoff, Opts, ?MAX_RETRY_BACKOFF),
    State = #state{
        ejabberd_host = Host,
        buckets = dict:new(),
        retry_timeout = RetryTimeout,
        backoff_interval = BackoffInterval,
        max_retry_backoff = MaxBackoffInterval
    },
    case create_connection_record(Hosts, State) of
        {error, Reason} -> {stop, {connection_init_error, Reason}};
        {ok, NewState} -> {ok, NewState}
    end.

%% handle_call({get_script, Password}, _From, State) ->
%%     Key = random:uniform(State#state.num_buckets - 1),
%%     Bkts = State#state.buckets,


handle_call({get_script, Password}, _From, State) ->
    case memc_get_connection(State, Password) of
        {ok, Mmc} ->
            {reply, {ok, Mmc}, State};
        {error, notfound} ->
            {reply, {error, notfound}, State}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_connection_record([H|T], State) ->
    ?INFO_MSG("create records: ~p", [H]),
    {Host, Port, Weight} = H,
    %% should we create the records regardless of success/failure?
    case memcached_conn:start_link([Host, Port]) of
        {ok, Conn} ->
            %erlang:monitor(process, Conn),
            Mmc = #mmc{ %% memcache record
                conn = Conn, %% FIXME: could make this a pool of connections?
                host = Host,
                port = Port,
                weight = Weight,
                init_time = now(),
                status = 0, %% last error message
                retry_interval = ?RETRY_TIMEOUT
            },
            NewDict = add_dict_records(Mmc, Weight, State#state.num_buckets, State#state.buckets, 0),
            create_connection_record(T, State#state{num_buckets = State#state.num_buckets + Weight, buckets = NewDict});
    %% In this case, we should go to a backoff, instead of stopping this
        {error, {Reason, DeadPid}} ->
            Mmc = #mmc{
                conn = DeadPid, %% so we can track crashed PIDs for restart strategy
                host = Host,
                port = Port,
                weight = Weight,
                init_time = now(),
                error = Reason,
                status = 1,
                retry_interval = ?RETRY_TIMEOUT
            },
            ?ERROR_MSG("Error connecting to memcache~n  Host: ~p Port: ~p Reason: ~p", [Host, Port, Reason]),
            NewDict = add_dict_records(Mmc, Weight, State#state.num_buckets, State#state.buckets, 0),
            create_connection_record(T, State#state{num_buckets = State#state.num_buckets + Weight, buckets = NewDict});
        O ->
            ?ERROR_MSG("Process stopped: ~p~n", [O]),
            {error, O}
    end;
create_connection_record([], State) ->
    {ok, State}.

add_dict_records(Conn, Weight, NumBkts, Dict, I) when Weight > 0 ->
    NewDict = dict:store(NumBkts + I, Conn, Dict),
    add_dict_records(Conn, Weight - 1, NumBkts, NewDict, I + 1);
add_dict_records(_Conn, Weight, _NumBkts, Dict, _I) when Weight =:= 0 ->
    Dict.

memc_get_connection(State, Key) ->
    %% need to check: if memc_hash == 0, memc_hash = 1
    Hash = case memc_hash(Key) of
               0 ->
                   1 rem State#state.num_buckets;
               H ->
                   H rem State#state.num_buckets
           end,

    Bkts = State#state.buckets,
    case dict:find(Hash, Bkts) of
        {ok, Value} ->
            {ok, Value};
        error ->
            {error, notfound}
    end.

memc_hash(Key) ->
    %% 32767 == 0x7fff
    ((erlang:crc32(Key) bsr 16) band 32767).