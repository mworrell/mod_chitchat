%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Fetch list of ChitChat rooms

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

-module(service_chitchat_rooms).
-author("Marc Worrell <marc@worrell.nl>").

-svc_title("Return the list of ChitChat rooms.").
-svc_needauth(false).

-export([process_get/2]).

process_get(_ReqData, Context) ->
    case z_mqtt_acl:is_allowed(publish, <<"~site/chitchat/status">>, Context) of
        true ->
            case mod_chitchat:rooms(Context) of
                {ok, Rooms} ->
                    {struct, [
                        {"status", "ok"},
                        {"rooms", {array, Rooms}}
                    ]};
                {error, _} ->
                    {struct, [
                        {"status", "error"},
                        {"error", "internal"}
                    ]}
            end;
        false ->
            {struct, [
                {"status", "error"},
                {"error", "eacces"}
            ]}
    end.
