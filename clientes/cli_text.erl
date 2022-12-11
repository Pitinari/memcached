-module(cli_text).
-export([put/3, del/2, get/2, take/2, stats/1, start/0, close/1]).

put(Sock, K, V) ->
  ok = gen_tcp:send(Sock, "PUT " ++ K ++ " " ++ V ++ "~n"), % PUT = 11
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

del(Sock, K) ->
  ok = gen_tcp:send(Sock, "DEL " ++ K ++ "~n"), % DEL = 12
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

get(Sock, K) ->
  ok = gen_tcp:send(Sock, "GET " ++ K ++ "~n"), % GET = 13
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

take(Sock, K) ->
  ok = gen_tcp:send(Sock, "TAKE " ++ K ++ "~n"), % TAKE = 14
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

stats(Sock) ->
  ok = gen_tcp:send(Sock, "STATS~n"), % STATS = 21
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

start() ->
  gen_tcp:connect("localhost", 889, [text, {packet, 0}]). % retorna {ok, Sock} si funciona bien

close(Sock) -> gen_tcp:close(Sock).
