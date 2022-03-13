-module(client_multi).

%% API
-export([start/3]).

start(MulticastAddress, Port, Name) ->
  {ok, Socket} = gen_udp:open(Port, [{reuseaddr,true}, {multicast_if, MulticastAddress}, {multicast_ttl,4}, {multicast_loop,false}, list, {active, false}]),
  inet:setopts(Socket,[{add_membership,{MulticastAddress,{0,0,0,0}}}]),
  Pid = spawn(fun() -> receiver_loop(Socket) end),
  gen_udp:controlling_process(Socket, Pid),
  register(sender_multi, spawn(fun() -> sender_process(Socket, MulticastAddress, Port, Name) end)).

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