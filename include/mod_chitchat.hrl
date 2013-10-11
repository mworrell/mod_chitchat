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


-record(room, {
		name,
		update,
		msg_ct,
		msg
	}).

-record(chitchat_status, {
		client_id,
		name,
		room,
		status
	}).

-record(chitchat_msg, {
		client_id,
		msg_id,
		time,
		name,
		msg
	}).


-define(STATUS_OFFLINE, 0).
-define(STATUS_AVAILABLE, 1).
-define(STATUS_AWAY, 2).
-define(STATUS_BUSY, 3).
