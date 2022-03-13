-module(client).
-author("marszub").

%% API
-export([start/5, send/1, send_udp/1, send_multi/1]).

start(ServerAddress, ServerPort, MulticastAddress, MyPort, Name) ->
  client_tcp:start(ServerAddress, ServerPort, Name),
  client_udp:start(ServerAddress, ServerPort, MyPort, Name),
  client_multi:start(MulticastAddress, ServerPort, Name),
  ok.

send(Message) ->
  sender_tcp ! Message.

send_udp(Message) ->
  sender_udp ! Message.

send_multi(Message) ->
  sender_multi ! Message.

