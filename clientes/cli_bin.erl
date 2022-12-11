-module(cli_bin).
-export([put/3, del/2, get/2, take/2, stats/1, start/0, close/1, code/1]).

sized(A) -> 
  B = binary:list_to_bin(integer_to_list(size(A))),
  case size(B) of
    1 -> {ok, <<0, 0, 0, B/binary>>};
    2 -> {ok, <<0, 0, B/binary>>};
    3 -> {ok, <<0, B/binary>>};
    4 -> {ok, B};
    _ -> {error, "error en sized, binario demasiado grande"} % devolver causa del error sin perder el programa
  end.

code(K) ->
  B = term_to_binary(K),
  case sized(B) of
    {ok, Coded} -> << Coded/binary, B/binary >>;
    {error, Reason} -> {error, Reason}
  end.

put(Sock, K, V) ->
  ok = gen_tcp:send(Sock, <<11, (code(K))/binary, (code(V))/binary >> ), % PUT = 11 % hacer una llamada a sized y ver que sea correcto
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

del(Sock, K) ->
  ok = gen_tcp:send(Sock, <<12, (code(K))/binary>> ),  % DEL = 12
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

get(Sock, K) ->
  ok = gen_tcp:send(Sock, <<13, (code(K))/binary>> ),  % GET = 13
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

take(Sock, K) ->
  ok = gen_tcp:send(Sock, <<13, (code(K))/binary>> ),  % TAKE = 14
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

stats(Sock) ->
  ok = gen_tcp:send(Sock, 21), % STATS = 21
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, 1) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.


start() ->
  gen_tcp:connect(889, [binary, {packet, 0}]). % retorna {ok, Sock} si funciona bien

close(Sock) -> gen_tcp:close(Sock).