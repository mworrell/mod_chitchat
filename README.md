mod_chitchat
============

Chat module for Zotonic - uses MQTT publish/subscribe

A multi-room browser based chat. Internally it uses MQTT for the message passing.
This will make it possible to use external MQTT client to connect to this chat.

It will need some extra messages for complete MQTT access, for example fetching the known rooms and the latest messages.

This chat has the following characteristics:

 * Push based communication over Websocket or Comet
 * Multiple rooms, can be added ad-hoc
 * Client names can be changed, and will be pushed to other clients
 * Postbacks are used to fetch the list of known rooms and the latest messages in a room
 * Defaults to public access, own access control can be added (usual Zotonic ACL mechanism)

After installing the module the chat can be found on the ``/chitchat`` url.
