-module(test).

%% API
-export([server/0, client/1]).

server() ->
  server:start(5201),
  inf_loop().

client(Num) ->
  client:start("localhost", 5201, 5201 + Num, {224, 0, 0, 251}, "Klient"),
  client:send("TCP"),
  client:send_udp("UDP"),
  client:send_multi("Multi"),
  inf_loop().

inf_loop() ->
  inf_loop().
