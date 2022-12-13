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

code(Data) ->
  Bin = term_to_binary(Data),
  Size = byte_size(Bin),
  BSize = <<Size:32>>,
  <<BSize/binary, Bin/binary>>.

put(Sock, K, V) ->
  case gen_tcp:send(Sock, <<?PUT, (code(K))/binary, (code(V))/binary >> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> ok;
            {ok, Code} -> io:fwrite("ok?, devolvió ~p ~n", [Code]);
            {error, Reason} -> io:fwrite("error en recv, devolvió ~p ~n", [Reason])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p ~n", [Reason])
  end.

del(Sock, K) ->
  case gen_tcp:send(Sock, <<?DEL, (code(K))/binary >> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> ok;
            {ok, <<?ENOTFOUND>>} -> enotfound;
            {ok, Code} -> io:fwrite("ok?, devolvió ~p ~n", [Code]);
            {error, Reason} -> io:fwrite("error en recv, devolvió ~p ~n", [Reason])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p ~n", [Reason])
  end.

get(Sock, K) ->
  case gen_tcp:send(Sock, <<?GET, (code(K))/binary>> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> case gen_tcp:recv(Sock, 4) of
                              {ok, Size} -> case gen_tcp:recv(Sock, binary:decode_unsigned(Size)) of
                                                  {ok, Packet} -> {ok, binary_to_term(Packet)};
                                                  {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
                                                end;
                              {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
                            end;
            {ok, <<?ENOTFOUND>>} -> enotfound;
            {ok, Code} -> io:fwrite("ok?, devolvió ~p ~n", [Code]);
            {error, Reason} -> io:fwrite("error en recv, devolvió ~p ~n", [Reason])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p ~n", [Reason])
  end.

take(Sock, K) ->
  case gen_tcp:send(Sock, <<?TAKE, (code(K))/binary>> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> case gen_tcp:recv(Sock, 4) of
                              {ok, Size} -> case gen_tcp:recv(Sock, binary:decode_unsigned(Size)) of
                                              {ok, Packet} -> {ok, binary_to_term(Packet)};
                                              {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
                                            end;
                              {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
                            end;
            {ok, <<?ENOTFOUND>>} -> enotfound;
            {ok, Code} -> io:fwrite("ok?, devolvió ~p ~n", [Code]);
            Cod -> io:fwrite("error en pedido, devolvió ~p~n", [Cod])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p~n", [Reason])
  end.

stats(Sock) ->
  case gen_tcp:send(Sock, <<?STATS>>) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> case gen_tcp:recv(Sock, 4) of
                              {ok, Size} -> case gen_tcp:recv(Sock, binary:decode_unsigned(Size)) of
                                              {ok, Packet} -> {ok, Packet};
                                              {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
                                            end;
                              {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason])
                            end;
            {ok, Code} -> io:fwrite("ok?, devolvió ~p ~n", [Code]);
            Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
          end;
    {error, Reason} -> io:fwrite("error en send, devolvió ~p~n", [Reason])
  end.

start() ->
  gen_tcp:connect("localhost", 889, [binary, {active, false}, {packet, raw}]).

close(Sock) -> gen_tcp:close(Sock).