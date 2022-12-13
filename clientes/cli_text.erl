-module(cli_text).
-export([put/3, del/2, get/2, take/2, stats/1, start/0, close/1]).

put(Sock, K, V) ->
  case gen_tcp:send(Sock, "PUT " ++ K ++ " " ++ V ++ "\n") of
    ok -> case gen_tcp:recv(Sock, 0) of
            {ok, Packet} -> {ok, Packet};
            {error, Reason} -> io:fwrite("error en recv, devolvió ~p~n", [Reason])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p~n", [Reason])
  end.

del(Sock, K) ->
  case gen_tcp:send(Sock, "DEL " ++ K ++ "\n") of
    ok -> case gen_tcp:recv(Sock, 0) of
            {ok, Packet} -> {ok, Packet};
            {error, Reason} -> io:fwrite("error en recv, devolvió ~p~n", [Reason])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p~n", [Reason])
  end.

get(Sock, K) ->
  case gen_tcp:send(Sock, "GET " ++ K ++ "\n") of
    ok -> case gen_tcp:recv(Sock, 0) of
            {ok, Packet} -> {ok, Packet};
            {error, Reason} -> io:fwrite("error en recv, devolvió ~p~n", [Reason])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p~n", [Reason])
  end.

take(Sock, K) ->
  case gen_tcp:send(Sock, "TAKE " ++ K ++ "\n") of
    ok -> case gen_tcp:recv(Sock, 0) of
            {ok, Packet} -> {ok, Packet};
            {error, Reason} -> io:fwrite("error en recv, devolvió ~p~n", [Reason])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p~n", [Reason])
  end.

stats(Sock) ->
  case gen_tcp:send(Sock, "STATS\n") of
    ok -> case gen_tcp:recv(Sock, 0) of
            {ok, Packet} -> {ok, Packet};
            {error, Reason} -> io:fwrite("error en recv, devolvió ~p~n", [Reason])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p~n", [Reason])
  end.

start() ->
  gen_tcp:connect("localhost", 888, [{active, false}, {packet, raw}]). 

close(Sock) -> gen_tcp:close(Sock).
