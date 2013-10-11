{% extends "base.tpl" %}

{# Popup page for chitchat #}

{% block title %}{_ Chit Chat _}{% endblock %}

{% block html_head_extra %}
    {% lib "css/chitchat.css" %}
{% endblock %}

{% block content %}
    <h1>
        {_ Chit Chat _}
        <input id="nick"
               class="input-xlarge do_autofocus" type="text" placeholder="Your name" 
               value="{{ q.name|force_escape|default:m.acl.user.name_first }}"
               tabindex="0"
        />
    </h1>

    <div id="chitchat" class="row-fluid">

        <div class="span2">
            <ul id="rooms" class="nav nav-list">
                <li class="nav-header">{_ Rooms _}</li>
            </ul>
            <a href="#addRoomModal" role="button" class="btn" data-toggle="modal">{_ Create Room _}</a>
        </div>

        <div class="span7">
            <div id="chat-container">
                <div id="chat">
                </div>
            </div>

            <form id="msgform">
                <input id="msg" class="input-block-level" name="msg" type="text" value="" 
                        placeholder="{{ _'Your message' }}"
                        autocomplete="off"
                        tabindex="1"
                />
                <button class="btn" type="submit">{_ Send _}</button>
            </form>
        </div>

        <div class="span3">
            <ul id="online" class="nav nav-list">
                <li class="nav-header">{_ Online _}</li>
            </ul>
        </div>

    </div>

    {# Templates #}
    <div class="hide">
        <div class="chitchat-msg" id="chitchat-msg">
            <div class="time"></div>
            <strong class="name"></strong>
            <div class="text"></div>
        </div>

        <ul>
            <li class="chitchat-person" id="chitchat-person">
                <a href="#">
                    <div class="name"></div>
                    <div class="room"></div>
                </a>
           </li>
       </ul>
    </div>

    {# Dialog: add room #}
    <form id="addRoomForm" class="form-horizontal">
        <div class="modal hide fade" id="addRoomModal">
            <div class="modal-header">
                <a type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</a>
                <h3>{_ Add a chat room _}</h3>
            </div>
            <div class="modal-body">
                <p>{_ After you added the room you will be transfer to the new room and others can join you there. _}</p>
                <div class="control-group">
                    <label class="control-label">{_ Name _}</label>
                    <div class="controls">
                        <input type="text" placeholder="" name="name" maxlength="20" />
                    </div>
                </div>
            </div>
            <div class="modal-footer">
                <a href="#" class="btn" data-dismiss="modal">{_ Cancel _}</a>
                <button type="submit" href="#" class="btn btn-primary">{_ Add Room _}</button>
            </div>
        </div>
    </form>

    {% javascript %}
        $('#msgform').submit(function(ev) {
            var msg = $('#msg').val();
            if (msg != '') {
                chitchat.message(msg);
                $('#msg').val('');
            }
            ev.preventDefault();
            return false;
        });

        $('#addRoomForm').submit(function(ev) {
            var name = $(this).find('[name=name]').val().trim();
            if (name != '') {
                chitchat.set_status(name, chitchat.AVAILABLE);
                $('#addRoomModal').modal('hide')
            }
            ev.preventDefault();
            return false;
        });

        $('#nick').on('change', function() {
            chitchat.set_name($(this).val());
        });
        if ($("#nick").val()) {
            chitchat.set_name($('#nick').val());
            $('#msg').focus();
        } else {
            $("#nick").focus();
        }

        $('#rooms').on('click', 'a', function(ev) {
            chitchat.set_status($(this).data('room'), chitchat.AVAILABLE);
            ev.preventDefault();
        });

        $('#online').on('click', 'a', function(ev) {
            chitchat.set_status($(this).find(".room").text(), chitchat.AVAILABLE);
            ev.preventDefault();
        });
    {% endjavascript %}

    {% stream %}
{% endblock %}

{% block _js_include_extra %}
    {% lib
             "js/ubf.js"
             "js/qlobber.js"
             "js/pubzub.js"
             "js/chitchat.js"
    %}
{% endblock %}
