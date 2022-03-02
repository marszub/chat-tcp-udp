-module(client).
-author("marszub").

%% API
-export([start/3, send/1]).

start(Address, Port, Name) ->
  {ok, Socket} = gen_tcp:connect(Address, Port, [binary, {packet, 0}]),
  ok = gen_tcp:send(Socket, Name ++ [2]),
  register(receiver, spawn(fun() -> receiver_process(Socket, []) end)),
  register(sender, spawn(fun() -> sender_process(Socket) end)).

receiver_process(Socket, Buffer) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, 0} ->
      io:format(lists:reverse(Buffer)),
      io:format("~n"),
      receiver_process(Socket, []);
    {ok, Byte} ->
      receiver_process(Socket, [Byte | Buffer]);
    {error, closed} ->
      ok
  end.

sender_process(Socket) ->
  receive
    Message ->
      gen_tcp:send(Socket, Message ++ [0]),
      sender_process(Socket)
  end.

send(Message) ->
  sender ! Message.

