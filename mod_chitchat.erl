%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Chat using MQTT

%% Copyright 2013 Marc Worrell
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

-module(mod_chitchat).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("ChitChat").
-mod_description("Chat using MQTT.").
-mod_prio(400).
-mod_depends([
        mod_mqtt
    ]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic.hrl").
-include_lib("emqtt/include/emqtt.hrl").
-include("include/mod_chitchat.hrl").

-record(state, {
        context,
        online,
        rooms,
        msg_ct
    }).

%% Keep the last 200 messages in a room
-define(ROOM_HISTORY, 200).

%% Keep inactive rooms for at most a week
-define(ROOM_ANCIENT, 7*24).


%% MQTT topic tree:
%%
%% /chitchat/status
%% /chitchat/msg/<room>
%% /chitchat/direct/<client_id>

%% MQTT subscriptions
-export([
    'mqtt:~site/chitchat/msg/+'/3,
    'mqtt:~site/chitchat/status'/3,

    event/2,

    observe_acl_is_allowed/2,
    online/1,
    rooms/1,
    messages/2
]).

%% Listen to the chit-chatting
'mqtt:~site/chitchat/msg/+'(Message, Pid, _Context) ->
    gen_server:cast(Pid, {msg, Message}).

'mqtt:~site/chitchat/status'(Message, Pid, _Context) ->
    gen_server:cast(Pid, {status, Message}).


%% Allow everybody to publish
observe_acl_is_allowed(#acl_is_allowed{object=#acl_mqtt{ words=[<<"site">>, _, <<"chitchat">>|_] }}, _Context) ->
    true;
observe_acl_is_allowed(_AclIsAllowed, _Context) ->
    undefined.


event(#postback_notify{message="rooms"}, Context) ->
    case z_mqtt_acl:is_allowed(publish, <<"~site/chitchat/status">>, Context) of
        true ->
            case rooms(Context) of
                {ok, Rooms} ->
                    {ok, Data} = z_ubf:encode(Rooms),
                    z_script:add_script([
                            <<"chitchat.fetch_rooms_cb('">>,
                                z_utils:js_escape(Data),
                            <<"');">>
                        ], Context); 
                {error, _} = Error ->
                    lager:error("Error fetching rooms ~p", [Error]),
                    z_render:growl("Server error while fetching rooms", Context)
            end;
        false ->
            z_render:growl("Access Denied", Context)
    end;
event(#postback_notify{message="messages"}, Context) ->
    case z_mqtt_acl:is_allowed(publish, <<"~site/chitchat/status">>, Context) of
        true ->
            Room = z_context:get_q("room", Context),
            case messages(Room, Context) of
                {ok, Ms} ->
                    {ok, Data} = z_ubf:encode(Ms),
                    z_script:add_script([
                            <<"chitchat.fetch_msg_cb('">>,
                                z_utils:js_escape(Room),
                                <<"','">>,
                                z_utils:js_escape(Data),
                            <<"');">>
                        ], Context); 
                {error, _} = Error ->
                    lager:error("Error fetching rooms ~p", [Error]),
                    z_render:growl("Server error while fetching rooms", Context)
            end;
        false ->
            z_render:growl("Access Denied", Context)
    end.

%% @doc Return the list of rooms
online(Context) ->
    Name = z_utils:name_for_host(?MODULE, Context),
    gen_server:call(Name, online).


%% @doc Return the list of rooms
rooms(Context) ->
    Name = z_utils:name_for_host(?MODULE, Context),
    gen_server:call(Name, rooms).

%% @doc Return the messages in a room
messages(Room, Context) ->
    Name = z_utils:name_for_host(?MODULE, Context),
    gen_server:call(Name, {msg, z_convert:to_binary(Room)}).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Name = z_utils:name_for_host(?MODULE, Context),
    gen_server:start_link({local,Name}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    {ok, #state{
        context=z_context:new(Context),
        online=[],
        rooms=dict:new(),
        msg_ct=0
    }}.

handle_call(online, _From, #state{online=Online} = State) ->
    {reply, {ok, Online}, State};

handle_call(rooms, _From, #state{rooms=Rooms, online=Online} = State) ->
    Rs = dict:fetch_keys(Rooms),
    Os = lists:map(fun(#chitchat_status{room=R}) -> R end, Online),
    {reply, {ok, lists:usort(Rs++Os)}, State};

handle_call({msg, RoomName}, _From, #state{rooms=Rooms} = State) ->
    case dict:find(RoomName, Rooms) of
        {ok, #room{msg=Msg}} ->
            {reply, {ok, Msg}, State};
        error ->
            {reply, {ok, []}, State}
    end;

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({status, #mqtt_msg{payload=Payload}}, State) ->
    case z_mqtt:payload_data(Payload) of
        {ok, #chitchat_status{client_id=ClientId, status=?STATUS_OFFLINE}} ->
            Cs = lists:keydelete(ClientId, #chitchat_status.client_id,  State#state.online),
            {noreply, State#state{online=Cs}};
        {ok, #chitchat_status{client_id=ClientId} = S} ->
            Cs = lists:keydelete(ClientId, #chitchat_status.client_id,  State#state.online),
            {noreply, State#state{online=[S|Cs]}};
        {ok, Other} ->
            lager:warning("Unexpected status: ~p", [Other]),
            {noreply, State};
        {error, _} = Error ->
            lager:error("Status decode error: ~p on ~p", [Error, Payload]),
            {noreply, State}
    end;

handle_cast({msg, #mqtt_msg{payload=Payload, topic=Topic}}, State) ->
    case z_mqtt:payload_data(Payload) of
        {ok, #chitchat_msg{} = Msg} ->
            RoomName = lists:last(binary:split(Topic, <<"/">>, [global])),
            Rooms1 = prune_rooms(do_msg(RoomName, Msg, State#state.rooms)),
            {noreply, State#state{rooms=Rooms1}};
        {ok, Other} ->
            lager:warning("Unexpected message: ~p", [Other]),
            {noreply, State};
        {error, _} = Error ->
            lager:error("Status decode error: ~p on ~p", [Error, Payload]),
            {noreply, State}
    end;

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

do_msg(RoomName, Msg, Rooms) ->
    do_msg1(dict:find(RoomName, Rooms), RoomName, Msg, Rooms).

do_msg1(error, RoomName, Msg, Rooms) ->
    R = #room{
        name=RoomName,
        update=z_utils:now_msec(),
        msg_ct=1,
        msg=[Msg]
    },
    dict:store(RoomName, R, Rooms);
do_msg1({ok, R}, _RoomName, Msg, Rooms) ->
    Ct = R#room.msg_ct+1,
    R1 = R#room{
            update=z_utils:now_msec(),
            msg_ct=Ct,
            msg=prune_msg(Ct, [Msg|R#room.msg])
        },
    dict:store(R1#room.name, R1, Rooms).

prune_msg(N, Ms) when N < ?ROOM_HISTORY ->
    Ms;
prune_msg(_N, Ms) ->
    lists:sublist(Ms, ?ROOM_HISTORY).

prune_rooms(Rooms) ->
    CutOff = z_utils:now_msec() - (?ROOM_ANCIENT * 24 * 3600 * 1000),
    dict:filter(fun(_Name, Room) ->
                    Room#room.update > CutOff
                end, Rooms). 

