-module(server_udp).

%% API
-export([start/1]).

start(Port) ->
  {ok, Socket} = gen_udp:open(Port, [list, {active, false}]),
  register(sender_udp, spawn(fun() -> sender_process(Socket, []) end)),
  Pid = spawn(fun() -> listen(Socket) end),
  gen_udp:controlling_process(Socket, Pid),
  Socket.

listen(Socket)->
  inet:setopts(Socket, [{active,once}]),
  receive
    {udp, Socket, Address, Port, Packet} ->
      io:format("Got udp: ~s~n", [Packet]),
      receiver_process({Address, Port}, Packet),
      listen(Socket);
    Else ->
      io:format("Else: ~w~n", [Else])
  end.

receiver_process(Sender, []) ->
  sender_udp ! {add, Sender};
receiver_process(Sender, [0]) ->
  sender_udp ! {remove, Sender};
receiver_process(Sender, Packet) ->
  sender_udp ! {send, Sender, Packet}.

sender_process(Socket, Clients) ->
  receive
    {add, ClientAddress} ->
      sender_process(Socket, [ClientAddress | Clients]);
    {remove, ClientAddress} ->
      sender_process(Socket, lists:delete(ClientAddress, Clients));
    {send, Sender, Message} ->
      send_broadcast(Socket, Clients, Sender, Message),
      sender_process(Socket, Clients);
    close ->
      ok
  end.

send_broadcast(_, [], _, _) ->
  ok;
send_broadcast(Socket, [Sender | Tail], Sender, Message) ->
  send_broadcast(Socket, Tail, Sender, Message);
send_broadcast(Socket, [{Address, Port} | Tail], Sender, Message) ->
  ok = gen_udp:send(Socket, Address, Port, Message),
  send_broadcast(Socket, Tail, Sender, Message).

