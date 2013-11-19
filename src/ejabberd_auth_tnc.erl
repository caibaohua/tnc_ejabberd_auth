%%%-------------------------------------------------------------------
%%% @author caibaohua
%%% @copyright (C) 2013, <The NetCircle>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十一月 2013 下午1:11
%%%-------------------------------------------------------------------
-module(ejabberd_auth_tnc).
-author("caibaohua").

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([
    start/1,
    stop/1,
    set_password/3,
    check_password/3,
    check_password/5,
    try_register/3,
    dirty_get_registered_users/0,
    get_vh_registered_users/1,
    get_vh_registered_users/2,
    get_vh_registered_users_number/1,
    get_vh_registered_users_number/2,
    get_password/2,
    get_password_s/2,
    is_user_exists/2,
    remove_user/2,
    remove_user/3,
    plain_password_required/0,
    store_type/0
]).

%% gen_server callbacks

-export([
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {service, ejabberd_host, options}).



%%%===================================================================
%%% API
%%%===================================================================

start(Host) ->
    Proc1 = gen_mod:get_module_proc(Host, ejabberd_auth_tnc),
    Proc2 = gen_mod:get_module_proc(Host, mod_auth_redis),
    Proc3 = gen_mod:get_module_proc(Host, mod_auth_memcache),
    ChildSpec1 = {Proc1, {ejabberd_auth_tnc, start_link, [Host]}, transient, 1000, worker, [ejabberd_auth_tnc]},
    ChildSpec2 = {Proc2, {mod_auth_redis, start_link, [Host]}, transient, 1000, worker, [mod_auth_redis]},
    ChildSpec3 = {Proc3, {mod_auth_memcache, start_link, [Host]}, transient, 1000, worker, [mod_auth_memcache]},
    supervisor:start_child(ejabberd_sup, ChildSpec1),
    supervisor:start_child(ejabberd_sup, ChildSpec2),
    supervisor:start_child(ejabberd_sup, ChildSpec3).

stop(Host) ->
    Proc1 = gen_mod:get_module_proc(Host, ejabberd_auth_tnc),
    Proc2 = gen_mod:get_module_proc(Host, mod_auth_redis),
    Proc3 = gen_mod:get_module_proc(Host, mod_auth_memcache),
    gen_server:call(Proc1, stop),
    gen_server:call(Proc2, stop),
    gen_server:call(Proc3, stop),
    supervisor:terminate_child(ejabberd_sup, Proc1),
    supervisor:terminate_child(ejabberd_sup, Proc2),
    supervisor:terminate_child(ejabberd_sup, Proc3),
    supervisor:delete_child(ejabberd_sup, Proc1),
    supervisor:delete_child(ejabberd_sup, Proc2),
    supervisor:delete_child(ejabberd_sup, Proc3).

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

terminate(_Reason, _State) -> ok.

init(Host) ->
    process_flag(trap_exit, true),
    Communities = parse_options(Host),
    Options = proplists:get_value(Host, Communities, []),
    Service = proplists:get_value(service, Options, []),
    State =  #state{service=Service, ejabberd_host=Host, options=Options},
    case get_service(State) of
        {error, Reason} ->
            ?ERROR_MSG("Create service error: ~p", [Reason]),
            {stop, {service_init_error, Reason}};
        {ok, NewState} ->
            ?INFO_MSG("Create service: ~p", [NewState]),
            {ok, NewState}
    end.

set_password(_User, _Host, _Password) ->
    {error, not_allowed}.

check_password(_User, _Host, _Password, _Digest, _DigestGen) ->
    check_password(_User, _Host, _Password).

check_password(_User, Host, Password) ->
    ?INFO_MSG("Check password for host: ~p", [Host]),
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    case gen_server:call(Proc, {check_password, Password}) of
        {ok, false} ->
            false;
        {ok, true} ->
            true;
        _ ->
            false
    end.


try_register(_User, _Server, _Password) -> {error, not_allowed}.

dirty_get_registered_users() -> [].

get_vh_registered_users(_Host) -> [].

get_vh_registered_users(_Host, _) -> [].

get_vh_registered_users_number(_Host) -> 0.

get_vh_registered_users_number(_Host, _) -> 0.

get_password(_User, _Server) -> false.

get_password_s(_User, _Server) -> <<"">>.

is_user_exists(_User, _Host) -> true.

remove_user(_User, _Host) -> {error, not_allowed}.

remove_user(_User, _Host, _Password) -> not_allowed.

plain_password_required() -> true.

store_type() -> external.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_call({check_password, Password}, _From, State) when State#state.service =:= "redis" ->
    Response = mod_auth_redis:check_password(Password),
    ?INFO_MSG("Handle call check_password redis: ~p", [Response]),
    {reply, {ok, Response}, State};
handle_call({check_password, Password}, _From, State) when State#state.service =:= "memcache" ->
    Response = mod_auth_memcache:check_password(Password),
    ?INFO_MSG("Handle call check_password memcache: ~p", [Response]),
    {reply, {ok, Response}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
get_service(State) when State#state.service =:= "redis" ->
    ?INFO_MSG("Get redis service", []),
    mod_auth_redis:start_link(State#state.ejabberd_host, State#state.options),
    {ok, State};
get_service(State) when State#state.service =:= "memcache" ->
    ?INFO_MSG("Get memcache service", []),
    mod_auth_memcache:start_link(State#state.ejabberd_host, State#state.options),
    {ok, State}.

parse_options(Host) ->
    case ejabberd_config:get_local_option({communities, Host}) of
        undefined  ->
            [];
        Communities -> Communities
    end.