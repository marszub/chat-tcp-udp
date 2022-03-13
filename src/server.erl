-module(server).
-author("marszub").

%% API
-export([start/1]).

start(Port) ->
  server_tcp:start(Port),
  server_udp:start(Port),
  ok.



