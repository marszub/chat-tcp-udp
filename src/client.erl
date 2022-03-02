-module(client).
-author("marszub").

%% API
-export([start/0]).

start() ->
  SomeHostInNet = "localhost", % to make it runnable on one machine
  {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678,
    [binary, {packet, 0}]),
  ok = gen_tcp:send(Sock, "Some Data"),
  ok = gen_tcp:close(Sock).
