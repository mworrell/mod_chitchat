/**
 * @author Marc Worrell <marc@worrell.nl>
 * @copyright 2013 Marc Worrell
 * @doc Chat using pubzub (MQTT)
 *
 * Copyright 2013 Marc Worrell
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

function Chitchat ()
{
    ubf.add_spec("chitchat_status", ["client_id", "name", "room", "status"]);
    ubf.add_spec("chitchat_msg", ["client_id", "msg_id", "time", "name", "msg"]);

    this._client_id = pubzub.unique_id();
    this._nick = '';
    this._alert = false;
    this._alert_toggle = false;
    this._document_title = document.title;

    // Available rooms, fetched from the server
    // Per room: { online: [], msg: [], new_msg: false } 
    this._room = {};
    this._online = {};
    this._status = {
        _record: "chitchat_status",
        client_id: this._client_id,
        name: "Nomen Nescio",
        room: "",
        status: 0
    };

    var self = this;
    var lastwill = {
        _record: "chitchat_status",
        client_id: self._client_id,
        status: self.OFFLINE
    };
    setTimeout(function() {
        pubzub.subscribe("/chitchat/status", function(topic, msg) { self.receive_status(topic, msg, false); });
        pubzub.subscribe("/chitchat/msg/+", function(topic, msg) { self.receive_msg(topic, msg); });
        pubzub.subscribe("/chitchat/direct/"+self._client_id, function(topic, msg) { self.receive_direct(topic, msg); });
        pubzub.lastwill("/chitchat/status", lastwill);
        window.onbeforeunload = function() {
            pubzub.publish("/chitchat/status", lastwill);
        };
        self.fetch_rooms_loop();
        setTimeout(function() {
            self.set_status("Lobby", chitchat.AVAILABLE);
        }, 100);
        self.check_alert();
    }, 100);
}

// Online status
Chitchat.prototype.OFFLINE = 0;
Chitchat.prototype.AVAILABLE = 1;
Chitchat.prototype.AWAY = 2;
Chitchat.prototype.BUSY = 3;

// Check every hour if any rooms disappeared (or magically appeared)
Chitchat.prototype.ROOM_POLL_PERIOD = 60*60*1000;

Chitchat.prototype.receive_msg = function(topic, msg) {
    var ws = topic.split('/');
    var room = ws[ws.length-1];
    if (this._status.room == room) {
        var $m = this.msg_element(msg.payload);
        var $cc = $('#chat-container');
        var do_scroll = msg.payload.client_id == this._client_id ||
                        $cc[0].scrollHeight - $cc.height() - 200 < $cc.scrollTop();
        $m.appendTo($('#chat')).hide().fadeIn();
        if (do_scroll) {
            $cc.scrollTop($cc[0].scrollHeight);
        }
        this.notify_user();
    } else {
        this.ensure_room(room);
        this._room[room].unread += 1;
        this.update_room(room);
    }
};

Chitchat.prototype.receive_status = function(topic, msg, is_direct) {
    if (typeof msg.payload.client_id == 'string' && typeof msg.payload.status == 'number') {
        var client_id = msg.payload.client_id;
        var rooms = [];

        if (typeof this._online[client_id] == 'object') {
            if (msg.payload.status == chitchat.OFFLINE) {
                var rm_room = this._online[client_id].room;
                this._room[rm_room].online = this._room[rm_room].online.filter(function(cid) { return cid != client_id; });
                delete this._online[client_id];
            } else {
                var new_room = msg.payload.room;
                this.ensure_room(new_room);
                var old_room = this._online[client_id].room;
                if (old_room != new_room) {
                    this._room[old_room].online = this._room[old_room].online.filter(function(cid) { return cid != client_id; });
                    this._room[new_room].online.push(client_id);
                    rooms.push(old_room);
                    rooms.push(new_room);
                }
                this._online[client_id] = msg.payload;
            }
        } else if (msg.payload.status != chitchat.OFFLINE) {
            this._online[client_id] = msg.payload;
            this.ensure_room(msg.payload.room);
            this._room[msg.payload.room].online.push(client_id);
            rooms.push(msg.payload.room);
            if (!is_direct && client_id != this._client_id) {
                this.send_direct(client_id, this._status);
            }
        }
        this.update_online();
        for (var i=0; i<rooms.length; i++) {
            this.update_room(rooms[i]);
        }
        if (client_id == this._client_id) {
            this.notify_user();
        }
    }
};

Chitchat.prototype.receive_direct = function(topic, msg) {
    if (msg.payload._record == 'chitchat_status') {
        this.receive_status(topic, msg, true);
    }
};

Chitchat.prototype.send_direct = function(client_id, msg) {
    pubzub.publish('/chitchat/direct/'+client_id, msg);
};


Chitchat.prototype.set_status = function(room, status) {
    room = room.replace(/[\/\+#]/, "-");
    var changed = this._status.room != room;
    this._status.room = room;
    this._status.status = status;
    pubzub.publish("/chitchat/status", this._status);
    if (changed) {
        this.fetch_msg(room);
    }
};

Chitchat.prototype.set_name = function(name) {
    name = name.trim();
    if (name === '') {
        name = 'Nomen Nescio';
    }
    this._status.name = name;
    pubzub.publish("/chitchat/status", this._status);
};

Chitchat.prototype.message = function(msg) {
    pubzub.publish("/chitchat/msg/"+this._status.room, {
            _record: "chitchat_msg",
            client_id: this._client_id,
            msg_id: pubzub.unique_id(),
            time: Math.round((new Date()).getTime() / 1000),
            name: this._status.name,
            msg: msg
    });
};

Chitchat.prototype.ensure_room = function(room) {
    if (typeof this._room[room] == 'undefined') {
        this._room[room] = { online: [], msg: [], unread: 0 };
        this.update_rooms();
    }
};

Chitchat.prototype.update_rooms = function() {
    this.ensure_room("Lobby");
    $("#rooms li:not(:first-child)").remove();
    this.append_room("Lobby");
    var ks = [];
    for (var k in this._room) {
        if (this._room.hasOwnProperty(k) && k != "Lobby") {
            ks.push(k);
        }
    }
    ks.sort();
    for (var i = 0; i<ks.length; i++) {
        this.append_room(ks[i]);
    }
};

Chitchat.prototype.update_room = function(name) {
    var $room = $('#rooms').filter(function() { return $(this).data('room') == name; });
    if ($room.length) {
        $room.find('.badge').text(this._rooms[name].unread+'');
        if (name == this._status.room) {
            $r.addClass('active');
        } else {
            $r.removeClass('active');
        }
    } else {
        this.update_rooms();
    }
};

Chitchat.prototype.append_room = function(name) {
    var r = this._room[name];
    var is_active = (name == this._status.room);
    if (is_active)
        r.unread = 0;

    $('<li/>', {
        class: is_active ? 'active' : ''
    })
    .append(
        $('<a/>', {
            href: '#'
        })
        .text(name)
        .data('room', name)
        .append(
            $('<span/>', {
                class: 'badge '+(r.unread?'':'hide')
            }).text(r.unread+'')
        )
    )
    .appendTo('#rooms');
};

Chitchat.prototype.fetch_rooms_loop = function() {
    z_notify("rooms", { z_delegate: "mod_chitchat" });
    var self = this;
    setTimeout(function() { self.fetch_rooms_loop(); }, this.ROOM_POLL_PERIOD);
};

Chitchat.prototype.fetch_rooms_cb = function(data) {
    var rooms = ubf.decode(data);
    var update = false;
    var i;
    for (i=0; i<rooms.length; i++) {
        if (typeof this._room[rooms[i]] == 'undefined') {
            this.ensure_room(rooms[i]);
            update = true;
        }
    }
    var ks = [];
    for (var k in this._room) {
        if (rooms.indexOf(k) == -1) {
            ks.push(k);
            update = true;
        }
    }
    for (i = 0; i<ks.length; i++) {
        delete this._room[ks[i]];
    }
    if (update) {
        this.update_rooms();
    }
};

Chitchat.prototype.fetch_msg = function(room) {
    $('#chat').children().fadeOut(function() { $(this).remove(); });
    z_notify("messages", { z_delegate: "mod_chitchat", room: room });
};

Chitchat.prototype.fetch_msg_cb = function(room, data) {
    if (room == this._status.room) {
        var msgs = ubf.decode(data);
        var $cc = $('#chat-container');
        var $m;
        var msg_id;
        $('#chat').html('');
        for (var i=0; i<msgs.length; i++) {
            if ($('#'+msgs[i].msg_id).length === 0) {
                $m = this.msg_element(msgs[i]);
                $m.prependTo($('#chat')).hide().fadeIn();
            }
        }
        $cc.scrollTop($cc[0].scrollHeight);
    }
};

Chitchat.prototype.update_online = function() {
    var ks = [];
    for (var k in this._online) {
        if (this._online.hasOwnProperty(k)) {
            ks.push(k);
            var p = this._online[k];
            var $k = $('#'+k);
            if ($k.length > 0) {
                $k.find('.name').text(p.name);
                $k.find('.room').text(p.room);
                $k.attr('class', 'chitchat-person status-'+p.status);
            } else {
                $k = $('#chitchat-person').clone();
                $k.attr('id', k)
                  .attr('class', 'chitchat-person status-'+p.status);
                $k.find('.name').text(p.name);
                $k.find('.room').text(p.room);
                $k.appendTo('#online').effect('highlight');
            }
        }
    }

    var rm = [];
    $('#online .chitchat-person').each(function() {
        if (ks.indexOf($(this).attr('id')) == -1) {
            rm.push($(this).attr('id'));
        }
    });

    for (var i = 0; i<rm.length; i++) {
        $('#'+rm[i])
            .effect('highlight')
            .fadeOut(300, function() { $(this).remove(); });
    }
};

Chitchat.prototype.msg_element = function(payload) {
    var t = new Date(payload.time * 1000);
    var hh = t.getHours();
    var ii = t.getMinutes();
    if (hh<10) hh='0'+hh;
    if (ii<10) ii='0'+ii;
    t = hh+':'+ii+', '+t.toLocaleDateString();

    var $m = $('#chitchat-msg').clone();
    $m.attr('id', payload.msg_id);
    if (typeof $.fn.emoticonize == 'function') {
        $m.find(".text").text(payload.msg).emoticonize({});
    } else {
        $m.find(".text").text(payload.msg);
    }
    $m.find(".time").text(t);
    if (typeof this._online[payload.client_id] == 'object') {
        $m.find(".name").text(this._online[payload.client_id].name);
    } else {
        $m.find(".name").text(payload.name);
    }
    if (payload.client_id == this._client_id) {
        $m.addClass('me');
    }
    return $m;
};

Chitchat.prototype.notify_user = function() {
    document.title = this.window_title();
    this._alert = true;
};

Chitchat.prototype.check_alert = function() {
    if (typeof document.hasFocus == "function") {
        if (this._alert_toggle) {
            document.title = this.window_title();
        }
        if (document.hasFocus()) {
            this._alert = false;
            this._alert_toggle = false;
        } else if (this._alert) {
            if (!this._alert_toggle) {
                document.title = "♦ " + this.window_title();
                this._alert_toggle = true;
            } else {
                this._alert_toggle = false;
            }
        }
        var self = this;
        setTimeout(function() { self.check_alert(); }, 1000);
    }
};

Chitchat.prototype.window_title = function() {
   return this._status.room + " - " + this._status.name;
};

