%%%-------------------------------------------------------------------
%%% @author johnliu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 九月 2019 11:30
%%%-------------------------------------------------------------------
-module(dashboard_worker).
-author("johnliu").
-include("dgiot_topo.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#{<<"sessionToken">> := SessionToken} = State) ->
    case shuwa_data:lookup({dashboard, SessionToken}) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    gen_server:start_link(?MODULE, [State], [])
            end;
        _Reason ->
            gen_server:start_link(?MODULE, [State], [])
    end;


start_link(State) ->
    lager:info("State ~p", [State]),
    ok.

stop(#{<<"sessionToken">> := SessionToken}) ->
    case shuwa_data:lookup({dashboard, SessionToken}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"data">> := Que, <<"sessionToken">> := SessionToken}]) ->
    shuwa_data:insert({dashboard, SessionToken}, self()),
    case length(Que) of
        0 ->
            erlang:send_after(300, self(), stop);
        _ ->
            Topic = <<"dashboard/", SessionToken/binary, "/heart">>,
            shuwa_mqtt:subscribe(Topic),
            erlang:send_after(30 * 1000, self(), heart),
            erlang:send_after(1000, self(), retry)
    end,
    {ok, #task{oldque = Que, newque = Que, freq = 1, sessiontoken = SessionToken}};

init(A) ->
    lager:info("A ~p ", [A]).

handle_call(stop, _From, State) ->
    erlang:garbage_collect(self()),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    erlang:garbage_collect(self()),
    {stop, Reason, State};

%% 任务结束
handle_info(retry, #task{newque = Que} = State) when length(Que) == 0 ->
    erlang:garbage_collect(self()),
    {stop, normal, State};

%% 定时触发抄表指令
handle_info(retry, State) ->
    {noreply, send_msg(State)};

%% 定时触发抄表指令
handle_info(heart, #task{heart = Heart} = State) ->
    erlang:send_after(30 * 1000, self(), heart),
    {noreply, State#task{heart = Heart + 1}};

%% 定时触发抄表指令
handle_info(heart, #task{heart = Heart} = State) when Heart > 3 ->
    {stop, normal, State};

%% 任务结束
handle_info({deliver, _, _Msg}, State) ->
    {noreply, State#task{heart = 0}};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_msg(#task{newque = Que} = State) ->
    Task = lists:nth(1, Que),
    dgiot_dashboard:do_task(Task, State),
    NewQue = lists:nthtail(1, Que),
    erlang:send_after(3 * 1000, self(), retry),
    State#task{newque = NewQue}.



