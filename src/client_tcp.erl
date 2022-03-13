-module(client_tcp).

%% API
-export([start/3]).

start(ServerAddress, ServerPort, Name) ->
  {ok, Socket} = gen_tcp:connect(ServerAddress, ServerPort, [list, {packet, 2}, {active, false}]),
  Pid = spawn(fun() -> receiver_loop(Socket, []) end),
  gen_tcp:controlling_process(Socket, Pid),
  gen_tcp:send(Socket, [2]),
  register(sender_tcp, spawn(fun() -> sender_process(Socket, Name) end)).

receiver_loop(Socket, Buffer) ->
  inet:setopts(Socket, [{active,once}]),
  receive
    {tcp,Socket,Data} ->
      NewBuffer = receiver_process(Data, Buffer),
      receiver_loop(Socket, NewBuffer);
    {tcp_closed, Socket} ->
      ok
  end.

receiver_process([], Buffer) ->
  Buffer;
receiver_process([0 | Tail], Buffer) ->
  io:format("~s~n", [lists:reverse(Buffer)]),
  receiver_process(Tail, []);
receiver_process([Byte | Tail], Buffer) ->
  receiver_process(Tail, [Byte | Buffer]).

sender_process(Socket, Name) ->
  receive
    Message ->
      gen_tcp:send(Socket, Name ++ ": " ++ Message ++ [0]),
      sender_process(Socket, Name)
  end.