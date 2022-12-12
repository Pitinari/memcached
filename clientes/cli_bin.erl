-module(cli_bin).
-export([put/3, del/2, get/2, take/2, stats/1, start/0, close/1, code/1]).

-define(PUT, 11).
-define(DEL, 12).
-define(GET, 13).
-define(TAKE, 14).
-define(STATS, 21).
-define(OK, 101).
-define(EINVAL, 111).
-define(ENOTFOUND, 112).
-define(EBINARY, 113).
-define(EBIG, 114).
-define(EUNK, 115).

sized(A) -> 
  B = integer_to_binary(size(A)),
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
  ok = gen_tcp:send(Sock, <<?PUT, (code(K))/binary, (code(V))/binary >> ), % PUT = 11 % hacer una llamada a sized y ver que sea correcto
  case gen_tcp:recv(Sock, 1) of
    ?OK -> ok;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end.

del(Sock, K) ->
  ok = gen_tcp:send(Sock, <<?DEL, (code(K))/binary>> ),
  case gen_tcp:recv(Sock, 1) of
    ?OK -> ok;
    ?ENOTFOUND -> enotfound;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end.

get(Sock, K) ->
  ok = gen_tcp:send(Sock, <<?GET, (code(K))/binary>> ),  
  case gen_tcp:recv(Sock, 1) of
    ?OK -> case gen_tcp:recv(Sock, 4) of
             {ok, Size} -> case gen_tcp:recv(Sock, binary_to_integer(Size)) of
                             {ok, Packet} -> {ok, Packet};
                             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
                           end;
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    ?ENOTFOUND -> enotfound;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end.

take(Sock, K) ->
  ok = gen_tcp:send(Sock, <<?TAKE, (code(K))/binary>> ),
  case gen_tcp:recv(Sock, 1) of
    ?OK -> case gen_tcp:recv(Sock, 4) of
             {ok, Size} -> case gen_tcp:recv(Sock, binary_to_integer(Size)) of
                             {ok, Packet} -> {ok, Packet};
                             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
                           end;
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    ?ENOTFOUND -> enotfound;
    Cod -> io:fwrite("error en pedido, devolvió ~p~n", [Cod])
  end,
  ok.

stats(Sock) ->
  ok = gen_tcp:send(Sock, ?STATS),
  case gen_tcp:recv(Sock, 1) of
    ?OK -> case gen_tcp:recv(Sock, 0) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

start() ->
  gen_tcp:connect("localhost", 889, [binary, {packet, raw}]).

close(Sock) -> gen_tcp:close(Sock).