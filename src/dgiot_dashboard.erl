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
-include("dgiot_topo.hrl").


post_dashboard(Args, #{<<"sessionToken">> := SessionToken} = _Context) ->
    supervisor:start_child(dashboard_task, [Args#{<<"sessionToken">> => SessionToken}]),
    timer:sleep(1000).
%%
%%<<"location">> =>
%%#{<<"__type">> => <<"GeoPoint">>,<<"latitude">> => 30.262441,
%%<<"longitude">> => 120.161324},
%%<<"name">> =>
%%<<229,164,170,233,152,179,232,131,189,230,142,167,229,136,
%%182,229,153,168,49,50,51,52,...>>,
%%<<"objectId">> => <<"5c413c7040">>,

do_task(#{<<"dataType">> := <<"map">>, <<"vuekey">> := Vuekey, <<"table">> := Table, <<"query">> := Query}, #task{sessiontoken = SessionToken}) ->
    case shuwa_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            NewResult =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"objectId">> := ObjectId, <<"name">> := Name, <<"location">> := #{<<"latitude">> := Latitude, <<"longitude">> := Longitude}} ->
                            Acc ++ [#{<<"objectid">> => ObjectId, <<"name">> => Name, <<"location">> => #{<<"longitude">> => Longitude, <<"latitude">> => Latitude}}];
                        _ ->
                            Acc
                    end
                            end, [], Results),
            Topic = <<"dashboard/", SessionToken/binary, "/post">>,
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"map">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => NewResult})),
            shuwa_mqtt:publish(self(), Topic, Base64);
        _ ->
            pass
    end;

%% shuwa_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>],<<"order">> => <<"-updatedAt">>, <<"limit">> => 10, <<"skip">> => 0}, [{"X-Parse-Session-Token", <<"r:3bcb41395765d5affefd549a0bcc6f0f">>}], [{from, rest}])
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := Vuekey, <<"table">> := Table, <<"query">> := Query}, #task{sessiontoken = SessionToken}) ->
    case shuwa_parse:query_object(Table, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Results} = NewResults} ->
            case Table of
                <<"Product">> ->
                    Products =
                        lists:foldl(fun(X, Acc) ->
                            case X of
                                #{<<"objectId">> := ObjectId} ->
                                    DeviceChild = getDevice(ObjectId, SessionToken),
                                    Acc ++ [X#{<<"deviceChild">> => DeviceChild}];
                                _ ->
                                    Acc
                            end
                                    end, [], Results),
                    Topic = <<"dashboard/", SessionToken/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => #{<<"count">> => Count, <<"results">> => Products}})),
                    shuwa_mqtt:publish(self(), Topic, Base64);
                _ ->
                    Topic = <<"dashboard/", SessionToken/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => NewResults})),
                    shuwa_mqtt:publish(self(), Topic, Base64)
            end;
        _ ->
            pass
    end;

do_task(Task, State) ->
    lager:info("Task ~p", [Task]),
    lager:info("State ~p", [State]),
    ok.

getDevice(<<"all">>, SessionToken) ->
    case shuwa_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"limit">> => 10}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"objectId">> := ObjectId, <<"name">> := Name} ->
                        Acc ++ [#{<<"objectid">> => ObjectId, <<"name">> => Name}];
                    _ ->
                        Acc
                end
                        end, [], Results);
        _ ->
            []
    end;

getDevice(ProductId, SessionToken) ->
    case shuwa_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"where">> => #{<<"product">> => ProductId}, <<"limit">> => 10}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"objectId">> := ObjectId, <<"name">> := Name} ->
                        Acc ++ [#{<<"objectid">> => ObjectId, <<"name">> => Name}];
                    _ ->
                        Acc
                end
                        end, [], Results);
        _ ->
            []
    end.
