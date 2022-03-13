-module(server_tcp).

%% API
-export([start/1]).

start(Port) ->
  {ok, ServerSocket} = gen_tcp:listen(Port, [list, {packet, 2}, {active, false}]),
  register(sender_tcp, spawn(fun() -> sender_process([]) end)),
  Pid = spawn(fun() -> listen(ServerSocket) end),
  gen_tcp:controlling_process(ServerSocket, Pid),
  ServerSocket.

listen(ServerSocket)->
  {ok, ClientSocket} = gen_tcp:accept(ServerSocket),
  io:format("New tcp connection~n"),
  Pid = spawn(fun() -> receiver_loop(ClientSocket, []) end),
  gen_tcp:controlling_process(ClientSocket, Pid),
  listen(ServerSocket).

sender_process(Clients) ->
  receive
    {add, ClientSocket} ->
      sender_process([ClientSocket | Clients]);
    {remove, ClientSocket} ->
      sender_process(lists:delete(ClientSocket, Clients));
    {send, Sender, Message} ->
      send_broadcast(Clients, Sender, Message),
      sender_process(Clients);
    close ->
      ok
  end.

send_broadcast([], _, _) ->
  ok;
send_broadcast([Sender | Tail], Sender, Message) ->
  send_broadcast(Tail, Sender, Message);
send_broadcast([Socket | Tail], Sender, Message) ->
  ok = gen_tcp:send(Socket, Message ++ [0]),
  send_broadcast(Tail, Sender, Message).

receiver_loop(ClientSocket, Buffer) ->
  inet:setopts(ClientSocket, [{active,once}]),
  receive
    {tcp,ClientSocket,Data} ->
      io:format("Got tcp: ~w~n", [Data]),
      NewBuffer = receiver_process(Data, Buffer, ClientSocket),
      receiver_loop(ClientSocket, NewBuffer);
    {tcp_closed, ClientSocket} ->
      io:format("Connection closed~n"),
      sender_tcp ! {remove, ClientSocket},
      ok
  end.

receiver_process([], Buffer, _) ->
  Buffer;
receiver_process([0 | Tail], Buffer, ClientSocket) ->
  sender_tcp ! {send, ClientSocket, lists:reverse(Buffer)},
  io:format("Sending ~w~n", [lists:reverse(Buffer)]),
  receiver_process(Tail, [], ClientSocket);
receiver_process([2 | Tail], Buffer, ClientSocket) ->
  sender_tcp ! {add, ClientSocket},
  io:format("Adding ~w~n", [lists:reverse(Buffer)]),
  receiver_process(Tail, [], ClientSocket);
receiver_process([Byte | Tail], Buffer, ClientSocket) ->
  receiver_process(Tail, [Byte | Buffer], ClientSocket).
