%%%-------------------------------------------------------------------
%%% @author caibaohua
%%% @copyright (C) 2013, <The NetCircle>
%%% @doc
%%%
%%% @end
%%% Created : 14. 十一月 2013 上午11:54
%%%-------------------------------------------------------------------
-module(mod_auth_redis).
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

-define(RETRY_TIMEOUT, 30000). %% 30 seconds


-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {
    ejabberd_host,
    buckets :: dict(),
    num_buckets = 0,
    retry_timeout :: integer()
}).

-record(connection, {
    host,
    port,
    database,
    weight,
    retry_interval
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Opts], []).

check_password(Password) ->
    case gen_server:call(?MODULE, {get_script}) of
        {ok, Conn} ->
            case eredis:start_link(Conn#connection.host, Conn#connection.port, Conn#connection.database, [], Conn#connection.retry_interval) of
                {ok, C} ->
                    ?INFO_MSG("Redis Conn OK: ~p", [Conn]),
                    case eredis:q(C, ["GET", Password]) of
                        {ok, undefined} ->
                            false;
                        {ok, Val} ->
                            auth_utils:auth_check(Val)
                    end;
                {connection_error, Reason} ->
                    ?ERROR_MSG("Redis Conn KO: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, notfound} ->
            ?ERROR_MSG("Redis connection not found. Check status of Redis server", []),
            false
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Opts]) ->
    Hosts = proplists:get_value(hosts, Opts, []),
    RetryTimeout = proplists:get_value(retry_timeout, Opts, ?RETRY_TIMEOUT),
    State = #state{
        ejabberd_host = Host,
        buckets = dict:new(),
        retry_timeout = RetryTimeout
    },
    case create_connection_record(Hosts, State) of
        {error, Reason} -> {stop, {connection_init_error, Reason}};
        {ok, NewState} -> {ok, NewState}
    end.

%% handle_call({get_script, Password}, _From, State) ->
%%     Key = random:uniform(State#state.num_buckets - 1),
%%     Bkts = State#state.buckets,


handle_call({get_script}, _From, State) ->
    Key = random:uniform(State#state.num_buckets)-1,
    case dict:find(Key, State#state.buckets) of
        {ok, Conn} ->
            {reply, {ok, Conn}, State};
        error ->
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
    {Host, Port, Database, Weight} = H,
    Conn = #connection{
        host = Host,
        port = Port,
        database = Database,
        weight = Weight,
        retry_interval = State#state.retry_timeout
    },
    NewDict = add_dict_records(Conn, Weight, State#state.num_buckets, State#state.buckets, 0),
    create_connection_record(T, State#state{num_buckets = State#state.num_buckets + Weight, buckets = NewDict});
create_connection_record([], State) ->
    {ok, State}.

add_dict_records(Conn, Weight, NumBkts, Dict, I) when Weight > 0 ->
    NewDict = dict:store(NumBkts + I, Conn, Dict),
    add_dict_records(Conn, Weight - 1, NumBkts, NewDict, I + 1);
add_dict_records(_Conn, Weight, _NumBkts, Dict, _I) when Weight =:= 0 ->
    Dict.
