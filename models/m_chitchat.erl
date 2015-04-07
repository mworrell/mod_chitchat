%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Chitchat model - store recent history in the database.

%% Copyright 2015 Marc Worrell
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


-module(m_chitchat).

-export([
    insert/3,
    list_rooms/1,
    list_messages/1,
    list_messages/2,
    prune/2,
    install/1
    ]).

-include("../include/mod_chitchat.hrl").

insert(Room, #chitchat_msg{msg_id=MsgId, name=Name, msg=Msg}, Context) ->
    MsgId1 = z_string:truncate(MsgId, 80),
    Name1 = z_string:truncate(Name, 80),
    Room1 = z_string:truncate(Room, 80),
    z_db:q("
        insert into chitchat (msg_id, name, room, message)
        values ($1, $2, $3, $4)",
        [MsgId1, Name1, Room1, Msg],
        Context).

list_rooms(Context) ->
    Rs = z_db:q("select distinct room from chitchat order by room", Context),
    [ R || {R} <- Rs ].

list_messages(Context) ->
    Rs = z_db:q("select msg_id, room, name, message, created
                 from chitchat
                 order by created",
                Context),
    lists:map(fun({MsgId, Room, Name, Message, Created}) ->
                Timestamp = z_datetime:datetime_to_timestamp(Created),
                {Room, #chitchat_msg{msg_id=MsgId, name=Name, msg=Message, time=Timestamp}}
              end,
              Rs).

list_messages(Room, Context) ->
    z_db:q("select name, message, created from chitchat where room = $1 order by created",
           [Room],
           Context).

prune(Hours, Context) ->
    z_db:q("delete from chitchat 
            where created < now() - interval '"++integer_to_list(Hours)++" hours'",
           Context).

install(Context) ->
    case z_db:table_exists(chitchat, Context) of
        false ->
            [] = z_db:q("
                create table chitchat (
                    id serial not null,
                    msg_id character varying (100) not null,
                    name character varying (100) not null,
                    room character varying (100) not null,
                    message text not null,
                    created timestamp with time zone not null default current_timestamp,

                    primary key (id)
                )
                ", Context),
            [] = z_db:q("create index chitchat_created on chitchat(created)", Context),
            z_db:flush(Context),
            ok;
        true ->
            ok
    end.
