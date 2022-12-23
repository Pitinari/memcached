-module(cli_bin).
-export([put/2, del/1, get/1, take/1, stats/0, start/0, start_aux/0, close/1, code/1]).

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

put(K, V) ->
	server ! {self(), put, K, V},
	receive
    Result -> Result
	end.
put(Sock, K, V) ->
  case gen_tcp:send(Sock, <<?PUT, (code(K))/binary, (code(V))/binary >> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, ?OK} -> ok;
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason}
  end.

del(K) ->
	server ! {self(), del, K},
	receive
		Result -> Result
	end.
del(Sock, K) ->
  case gen_tcp:send(Sock, <<?DEL, (code(K))/binary >> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> {ok, ok};
            {ok, <<?ENOTFOUND>>} -> {ok, enotfound};
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason}
  end.

get(K) ->
	server ! {self(), get, K},
	receive
		Result -> Result
	end.
get(Sock, K) ->
  case gen_tcp:send(Sock, <<?GET, (code(K))/binary>> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> case gen_tcp:recv(Sock, 4) of
                              {ok, Size} -> case gen_tcp:recv(Sock, binary:decode_unsigned(Size)) of
                                                  {ok, Packet} -> {ok, binary_to_term(Packet)};
                                                  {error, Reason} -> {error, Reason}
                                                end;
                              {error, Reason} -> {error, Reason}
                            end;
            {ok, <<?ENOTFOUND>>} -> {ok, enotfound};
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason}
  end.

take(K) ->
	server ! {self(), take, K},
	receive
		Result -> Result
	end.
take(Sock, K) ->
  case gen_tcp:send(Sock, <<?TAKE, (code(K))/binary>> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> case gen_tcp:recv(Sock, 4) of
                              {ok, Size} -> case gen_tcp:recv(Sock, binary:decode_unsigned(Size)) of
                                              {ok, Packet} -> {ok, binary_to_term(Packet)};
                                              {error, Reason} -> {error, Reason}
                                            end;
                              {error, Reason} -> {error, Reason}
                            end;
            {ok, <<?ENOTFOUND>>} -> {ok, enotfound};
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason}
  end.

stats() ->
	server ! {self(), stats},
	receive
		Result -> Result
	end.
stats(Sock) ->
  case gen_tcp:send(Sock, <<?STATS>>) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> case gen_tcp:recv(Sock, 4) of
                              {ok, Size} -> case gen_tcp:recv(Sock, binary:decode_unsigned(Size)) of
                                              {ok, Packet} -> {ok, Packet};
                                              {error, Reason} -> {error, Reason} 
                                            end;
                              {error, Reason} -> {error, Reason} 
                            end;
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason} 
  end.

wait_for_clients(Socket) ->
	receive
		{PId, put, K, V} -> PId ! put(Socket, K, V),
												wait_for_clients(Socket);
		{PId, del, K} -> PId ! del(Socket, K),
										 wait_for_clients(Socket);
		{PId, get, K} -> PId ! get(Socket, K),
										 wait_for_clients(Socket);
		{PId, take, K} -> PId ! take(Socket, K),
											wait_for_clients(Socket);
		{PId, stats} -> PId ! stats(Socket),
										wait_for_clients(Socket);
		_ -> wait_for_clients(Socket)
	end.

start_aux() ->
	case gen_tcp:connect("localhost", 889, [binary, {active, false}, {packet, raw}]) of
		{ok, Socket} -> register(server, self()),
										wait_for_clients(Socket);
		Error-> Error
	end.
start() ->
	spawn(cli_bin, start_aux, []).	

close(Sock) -> gen_tcp:close(Sock).