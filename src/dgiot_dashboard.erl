%%%-------------------------------------------------------------------
%%% @author stoneliu
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% solarPower
%%% @end
%%% Created : 07. 五月 2021 12:00
%%%-------------------------------------------------------------------
-module(dgiot_dashboard).
-author("stoneliu").
-export([post_dashboard/2, do_task/2]).

post_dashboard(Args, #{<<"sessionToken">> := SessionToken} = _Context) ->
    supervisor:start_child(dashboard_task, [Args#{<<"sessionToken">> => SessionToken}]),
    timer:sleep(1000).


do_task(#{<<"dataType">> := <<"map">>} = Task, State) ->
    lager:info("Task ~p", [Task]),
    ok;


do_task(#{<<"dataType">> := <<"card">>, <<"key">> := <<"app"/utf8>>} = Task, State) ->

    lager:info("Task ~p", [Task]),
    ok;


do_task(Task, State) ->
    lager:info("Task ~p", [Task]),
    ok.


