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
  spawn(fun() -> receiver_process(ClientSocket, []) end),
  listen(ServerSocket).

sender_process(ClientDict) ->
  receive
    {add, ClientSocket, Nick} ->
      sender_process(dict:store(ClientSocket, Nick, ClientDict));
    {remove, ClientSocket} ->
      sender_process(dict:erase(ClientSocket, ClientDict));
    {send, Sender, Message} ->
      send_broadcast(dict:to_list(ClientDict), Sender, Message),
      sender_process(ClientDict);
    close ->
      ok
  end.

send_broadcast([], _, _) ->
  ok;
send_broadcast([{Sender, _} | Tail], Sender, Message) ->
  send_broadcast(Tail, Sender, Message);
send_broadcast([{Socket, Name} | Tail], Sender, Message) ->
  ok = gen_tcp:send(Socket, Name ++ ": " ++ Message),
  send_broadcast(Tail, Sender, Message).

receiver_process(ClientSocket, Buffer)->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, 0} ->
      sender ! {send, ClientSocket, lists:reverse([0 | Buffer])},
      receiver_process(ClientSocket, []);
    {ok, 2} ->
      sender ! {add, ClientSocket, lists:reverse(Buffer)},
      receiver_process(ClientSocket, []);
    {ok, Byte} ->
      receiver_process(ClientSocket, [Byte | Buffer]);
    {error, closed} ->
      sender ! {remove, ClientSocket}
  end.
