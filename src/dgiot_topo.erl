%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(dgiot_topo).
-author("johnliu").

-export([start_http/0, docroot/0, get_topo/2, send_topo/3, get_Product/0, get_name/3, put_topo/2, get_konva_thing/2, edit_konva/2]).

start_http() ->
    Port = application:get_env(?MODULE, port, 6081),
    DocRoot = docroot(),
    shuwa_http_server:start_http(?MODULE, Port, DocRoot).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = shuwa_httpc:url_join([Dir, "/priv/"]),
    Root ++ "www".

get_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"devaddr">> := Devaddr
    } = Arg,
    case shuwa_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children} = Stage} = Konva}}} when length(Children) > 0 ->
            case Devaddr of
                undefined ->
                    NewChildren1 = get_children(ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}}};
                _ ->
                    DeviceId = shuwa_parse:get_deviceid(ProductId, Devaddr),
                    NewChildren1 = get_children(ProductId, Children, DeviceId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren1}}}}
            end;
        _ ->
            {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
    end.

get_konva_thing(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"shapeid">> := Shapeid
    } = Arg,
    case shuwa_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}, <<"thing">> := #{<<"properties">> := Properties}}} ->
            put({self(), shapeids}, []),
            get_children(ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>),
            Shapids = get({self(), shapeids}),
            Nobound =
                lists:foldl(fun(Prop, Acc) ->
                    Identifier = maps:get(<<"identifier">>, Prop),
                    case lists:member(Identifier, Shapids) of
                        false ->
                            Acc ++ [Prop];
                        true ->
                            Acc
                    end
                            end, [], Properties),
            KonvaThing =
                lists:foldl(fun(Prop, Acc) ->
                    Identifier = maps:get(<<"identifier">>, Prop),
                    case Shapeid of
                        Identifier ->
                            Prop;
                        _ ->
                            Acc
                    end
                            end, #{}, Properties),
            {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => #{<<"nobound">> => Nobound, <<"konvathing">> => KonvaThing}}};
        _ ->
            {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
    end.

edit_konva(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"shapeid">> := Shapeid,
        <<"identifier">> := Identifier,
        <<"name">> := Name
    } = Arg,
    case shuwa_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children} = Stage} = Konva} = Config}} ->
            put({self(), shapeids}, []),
            NewChildren = get_children(ProductId, Children, ProductId, ProductId, Shapeid, Identifier, Name),
            NewConfig = Config#{<<"konva">> => Konva#{<<"Stage">> => Stage#{<<"children">> => NewChildren}}},
            case shuwa_parse:update_object(<<"Product">>, ProductId, #{<<"config">> => NewConfig}) of
                {ok, Message} ->
                    {ok, #{<<"code">> => 200, <<"message">> => Message}};
                {error, Message} ->
                    {ok, Message};
                _ ->
                    {ok, #{<<"code">> => 500, <<"message">> => <<"error">>}}
            end;
        _ ->
            {ok, #{<<"code">> => 101, <<"message">> => <<ProductId/binary, " not found">>}}
    end.

get_children(ProductId, Children, DeviceId, KonvatId, Shapeid, Identifier, Name) ->
    lists:foldl(fun(X, Acc) ->
        #{<<"attrs">> := Attrs, <<"className">> := ClassName} = X,
        X1 =
            case maps:find(<<"children">>, X) of
                error ->
                    X#{<<"attrs">> => get_attrs(ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name)};
                {ok, SubChildren} ->
                    X#{<<"attrs">> => get_attrs(ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name),
                        <<"children">> => get_children(ProductId, SubChildren, DeviceId, KonvatId, Shapeid, Identifier, Name)}
            end,
        Acc ++ [X1]
                end, [], Children).

get_attrs(ProductId, ClassName, Attrs, DeviceId, KonvatId, Shapeid, Identifier, Name) ->
    case ClassName of
        <<"Layer">> ->
            Attrs;
        <<"Group">> ->
            Attrs;
        _ ->
            Id = maps:get(<<"id">>, Attrs),
            case ProductId of
                KonvatId ->
                    case Id of
                        Shapeid ->
                            Attrs#{<<"id">> => Identifier, <<"text">> => Name};
                        _ ->
                            Attrs
                    end;
                DeviceId ->
                    case get({self(), shapeids}) of
                        undefined ->
                            put({self(), shapeids}, [Id]);
                        List ->
                            put({self(), shapeids}, List ++ [Id])
                    end,
                    shuwa_data:insert({shapetype, shuwa_parse:get_shapeid(ProductId, Id)}, ClassName),
                    Attrs;
                _ ->
                    Attrs#{<<"id">> => shuwa_parse:get_shapeid(DeviceId, maps:get(<<"id">>, Attrs))}
            end
    end.

%% #{<<"Arcel">>=> 1,<<"Flow">> => 1.2} => ShapeId = md5(<<DeviceId/binary,"Arcel">>)
%%{
%%"konva":{
%%    [
%%                {
%%                "id":[shapeid],
%%                "text":"16",
%%                "type":"text",
%%                },
%%                {
%%                "id":[shapeid],
%%                "text":"16",
%%                "type":"Image",
%%                }
%%        ]
%%    }
%%}  shuwa_data:get({product, <<"16cf2bf9f7energy">>})
%% dgiot_topo:send_topo(<<"9b5c1a3ed5">>, <<"001">>, #{<<"Acrel">> => 10,<<"current">> => 20,<<"current">> => 30}).
send_topo(ProductId, Devaddr, Payload) ->
    DeviceId = shuwa_parse:get_deviceid(ProductId, Devaddr),
    Shape =
        maps:fold(fun(K, V, Acc) ->
            Text = get_name(ProductId, K, shuwa_utils:to_binary(V)),
            Type =
                case shuwa_data:get({shapetype, shuwa_parse:get_shapeid(ProductId, K)}) of
                    not_find ->
                        <<"text">>;
                    Type1 ->
                        Type1
                end,
            Acc ++ [#{<<"id">> => shuwa_parse:get_shapeid(DeviceId, K), <<"text">> => Text, <<"type">> => Type}]
                  end, [], Payload),
    Pubtopic = <<"thing/", DeviceId/binary, "/post">>,
    Base64 = base64:encode(jsx:encode(#{<<"konva">> => Shape})),
    shuwa_mqtt:publish(self(), Pubtopic, Base64).

put_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"devaddr">> := Devaddr,
        <<"base64">> := Base64
    } = Arg,
    DeviceId = shuwa_parse:get_deviceid(ProductId, Devaddr),
    Pubtopic = <<"thing/", DeviceId/binary, "/post">>,
    shuwa_mqtt:publish(self(), Pubtopic, Base64),
    {ok, <<"Success">>}.

get_name(ProductId, K, V) ->
    case shuwa_data:get({product, <<ProductId/binary, K/binary>>}) of
        not_find ->
            V;
        {Name, Type, Unit} when Type =:= <<"float">> orelse Type =:= <<"double">> ->
            NewV = shuwa_utils:to_binary(shuwa_utils:to_float(V, 3)),
            <<Name/binary, ": ", NewV/binary, " ", Unit/binary>>;
        {Name, _Type, Unit} ->
            <<Name/binary, ":", V/binary, " ", Unit/binary>>
    end.

get_Product() ->
    case shuwa_parse:query_object(<<"Product">>, #{<<"skip">> => 0}) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, _Acc) ->
                case X of
                    #{<<"objectId">> := ProductId, <<"config">> := #{<<"konva">> := #{<<"Stage">> := #{<<"children">> := Children}}}, <<"thing">> := #{<<"properties">> := Properties}} ->
                        lists:map(fun(P) ->
                            DataType = maps:get(<<"dataType">>, P),
                            Type = maps:get(<<"type">>, DataType),
                            Specs = maps:get(<<"specs">>, DataType),
                            Unit = maps:get(<<"unit">>, Specs, <<"">>),
                            Identifier = maps:get(<<"identifier">>, P),
                            Name = maps:get(<<"name">>, P),
                            shuwa_data:insert({product, <<ProductId/binary, Identifier/binary>>}, {Name, Type, Unit}),
                            shuwa_data:insert({thing, <<ProductId/binary, Identifier/binary>>}, P)
                                  end, Properties),
                        get_children(ProductId, Children, ProductId, <<"KonvatId">>, <<"Shapeid">>, <<"Identifier">>, <<"Name">>);
                    _ ->
                        pass
                end
                        end, [], Results);
        _ ->
            pass
    end.

