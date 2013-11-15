%%%-------------------------------------------------------------------
%%% @author caibaohua
%%% @copyright (C) 2013, <The NetCircle>
%%% @doc
%%%
%%% @end
%%% Created : 15. 十一月 2013 下午1:50
%%%-------------------------------------------------------------------
-module(auth_utils).
-author("caibaohua").

%% API
-export([auth_check/1]).


auth_check(V) ->
    case re:run(V, "/^.*symfony/user/sfUser/authenticated\|b:1.*$", []) of
        nomatch ->
            false;
        _ ->
            true
    end.
