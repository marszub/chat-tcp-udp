-module(server).
-author("marszub").

%% API
-export([start/1]).


start(Port) ->
  {ok, ServerSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
  ClientDict = dict:new(),
  sender = spawn(fun() -> sender_process(ClientDict) end),

  ok = listen(ServerSocket),
  ok = gen_tcp:close(ServerSocket).

listen(ServerSocket)->
  {ok, ClientSocket} = gen_tcp:accept(ServerSocket),
  Pid = spawn(?MODULE, receiver_process, [ClientSocket]),
  listen(ServerSocket).

sender_process(ClientDict) ->
  receive
    {add, ClientSocket, Nick} ->
      sender_process();
    {remove, ClientSocket} ->
      sender_process();
    {send, Message} ->
      sender_process()
  end.

receiver_process(ClientSocket)->
  ok = gen_tcp:close(ClientSocket).

do_recv(Sock, Bs) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, B} ->
      do_recv(Sock, [Bs, B]);
    {error, closed} ->
      {ok, list_to_binary(Bs)}
  end.
