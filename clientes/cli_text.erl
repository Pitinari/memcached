-module(cli_text).
-export([put/3, del/2, get/2, take/2, stats/1, start/0, close/1]).

put(Sock, K, V) ->
  case gen_tcp:send(Sock, "PUT " ++ K ++ " " ++ V ++ "~n") of
    ok -> gen_tcp:recv(Sock, 0);
    {error, Reason} -> {error, Reason}
  end.

del(Sock, K) ->
  gen_tcp:send(Sock, "DEL " ++ K ++ "~n"),
  gen_tcp:recv(Sock, 0).

get(Sock, K) ->
  gen_tcp:send(Sock, "GET " ++ K ++ "~n"),
  gen_tcp:recv(Sock, 0).

take(Sock, K) ->
  gen_tcp:send(Sock, "TAKE " ++ K ++ "~n"),
  gen_tcp:recv(Sock, 0).

stats(Sock) ->
  gen_tcp:send(Sock, "STATS~n"),
  gen_tcp:recv(Sock, 0).

start() ->
  gen_tcp:connect("localhost", 888, [{packet, 0}]). 

close(Sock) -> gen_tcp:close(Sock).
