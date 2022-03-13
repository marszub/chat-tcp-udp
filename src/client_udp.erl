-module(client_udp).

%% API
-export([start/4]).

start(ServerAddress, ServerPort, MyPort, Name) ->
  {ok, Socket} = gen_udp:open(MyPort, [list, {active, false}]),
  Pid = spawn(fun() -> receiver_loop(Socket) end),
  gen_udp:controlling_process(Socket, Pid),
  gen_udp:send(Socket, ServerAddress, ServerPort, []),
  register(sender_udp, spawn(fun() -> sender_process(Socket, ServerAddress, ServerPort, Name) end)).

receiver_loop(Socket) ->
  inet:setopts(Socket, [{active,once}]),
  receive
    {udp, Socket, _, _, Packet} ->
      io:format("~s~n", [Packet]),
      receiver_loop(Socket)
  end.

sender_process(Socket, Address, Port, Name) ->
  receive
    Message ->
      gen_udp:send(Socket, Address, Port, Name ++ ": " ++ Message),
      sender_process(Socket, Address, Port, Name)
  end.