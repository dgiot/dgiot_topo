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

do_task(#{<<"dataType">> := <<"map">>, <<"id">> := Id, <<"key">> := Key} = Task, #task{sessiontoken = SessionToken}) ->
    case shuwa_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"name">>, <<"location">>], <<"limit">> => 100}) of
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
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"map">>, <<"id">> => Id, <<"key">> => Key, <<"value">> => NewResult})),
            shuwa_mqtt:publish(self(), Topic, Base64);
        _ ->
            pass
    end;

do_task(#{<<"dataType">> := <<"card">>, <<"id">> := Id, <<"key">> := Key, <<"query">> := #{<<"keys">> := Keys, <<"where">> := Where}}, #task{sessiontoken = SessionToken}) ->
    case shuwa_parse:query_object(Key, #{<<"keys">> => Keys ++ [<<"objectId">>], <<"where">> => Where, <<"limit">> => 1}) of
        {ok, #{<<"count">> := Count, <<"results">> := Results}} ->
            case Key of
                <<"Product">> ->
                    Products =
                        lists:foldl(fun(X, Acc) ->
                            case X of
                                #{<<"objectId">> := ObjectId} ->
                                    UpdatedAt = maps:get(<<"updatedAt">>, X, <<>>),
                                    Name = maps:get(<<"name">>, X, <<>>),
                                    Category = maps:get(<<"category">>, X, <<>>),
                                    Desc = maps:get(<<"desc">>, X, <<>>),
                                    Icon = maps:get(<<"icon">>, X, <<>>),
                                    DeviceChild = getDevice(ObjectId),
                                    Acc ++ [#{<<"objectid">> => ObjectId, <<"name">> => Name, <<"updatedAt">> => UpdatedAt, <<"category">> => Category, <<"desc">> => Desc, <<"icon">> => Icon, <<"deviceChild">> => DeviceChild}];
                                _ ->
                                    Acc
                            end
                                    end, [], Results),
                    Topic = <<"dashboard/", SessionToken/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"id">> => Id, <<"key">> => Key, <<"value">> => #{<<"count">> => Count, <<"results">> => Products}})),
                    shuwa_mqtt:publish(self(), Topic, Base64);
                _ ->
                    Topic = <<"dashboard/", SessionToken/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"id">> => Id, <<"key">> => Key, <<"value">> => Count})),
                    shuwa_mqtt:publish(self(), Topic, Base64)
            end;
        _ ->
            pass
    end;

do_task(Task, State) ->
    lager:info("Task ~p", [Task]),
    lager:info("State ~p", [State]),
    ok.

getDevice(ProductId) ->
    shuwa_parse:query_object(<<"Device">>, #{<<"limit">> => 1, <<"where">> => #{<<"product">> => <<"30a01ed480">>}}),
    case shuwa_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"name">>], <<"where">> => #{<<"product">> => ProductId}, <<"limit">> => 100}) of
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
