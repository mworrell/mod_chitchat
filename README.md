ChitChat
========

mod_chitchat is a multi-room chat module for Zotonic.

Internally MQTT is used for the message passing. 
This makes it possible to use external MQTT clients to connect to this chat.

ChitChat has the following characteristics:

 * Push based communication over Websocket or Comet
 * Multiple rooms, can be added ad-hoc
 * Client names can be changed, and will be pushed to other clients
 * Postbacks are used to fetch the list of known rooms and the latest messages in a room
 * Defaults to public access, own access control can be added (usual Zotonic ACL mechanism)
 * No logging, only a limited number of messages is retained

After installing the module the chat can be found on the ``/chitchat`` url.

Known issues
------------

 * Some extra messages are needed for complete MQTT-only usage, for example fetching the known rooms and the latest messages.
 * Right now every client is connected to the *fire hose*, this will pose problems if many people are active.

Future plans
------------

 * Away/Busy state support
 * Desktop notifications on new messages if window is not in focus
 * Display smileys
 * Exchange files
