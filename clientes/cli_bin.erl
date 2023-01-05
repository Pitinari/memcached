-module(cli_bin).
-export([put/3, del/2, get/2, take/2, stats/1, start/0, close/1, wait_for_clients/1]).

-define(PUT, 11).
-define(DEL, 12).
-define(GET, 13).
-define(TAKE, 14).
-define(STATS, 21).
-define(OK, 101).
-define(ENOTFOUND, 112).

code(Data) ->
  Bin = term_to_binary(Data),
  Size = byte_size(Bin),
  BSize = <<Size:32>>,
  <<BSize/binary, Bin/binary>>.

put(Server, K, V) ->
	Server ! {self(), put, K, V},
	receive
    Result -> Result
	end.
put_(Sock, K, V) ->
  case gen_tcp:send(Sock, <<?PUT, (code(K))/binary, (code(V))/binary >> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> ok;
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason}
  end.

del(Server, K) ->
	Server ! {self(), del, K},
	receive
		Result -> Result
	end.
del_(Sock, K) ->
  case gen_tcp:send(Sock, <<?DEL, (code(K))/binary >> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> ok;
            {ok, <<?ENOTFOUND>>} -> enotfound;
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason}
  end.

get(Server, K) ->
	Server ! {self(), get, K},
	receive
		Result -> Result
	end.
get_(Sock, K) ->
  case gen_tcp:send(Sock, <<?GET, (code(K))/binary>> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> case gen_tcp:recv(Sock, 4) of
                              {ok, Size} -> case gen_tcp:recv(Sock, binary:decode_unsigned(Size)) of
                                                  {ok, Packet} -> {ok, binary_to_term(Packet)};
                                                  {error, Reason} -> {error, Reason}
                                                end;
                              {error, Reason} -> {error, Reason}
                            end;
            {ok, <<?ENOTFOUND>>} -> enotfound;
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason}
  end.

take(Server, K) ->
	Server ! {self(), take, K},
	receive
		Result -> Result
	end.
take_(Sock, K) ->
  case gen_tcp:send(Sock, <<?TAKE, (code(K))/binary>> ) of
    ok -> case gen_tcp:recv(Sock, 1) of
            {ok, <<?OK>>} -> case gen_tcp:recv(Sock, 4) of
                              {ok, Size} -> case gen_tcp:recv(Sock, binary:decode_unsigned(Size)) of
                                              {ok, Packet} -> {ok, binary_to_term(Packet)};
                                              {error, Reason} -> {error, Reason}
                                            end;
                              {error, Reason} -> {error, Reason}
                            end;
            {ok, <<?ENOTFOUND>>} -> enotfound;
            {ok, Code} -> {ok, Code};
            {error, Reason} -> {error, Reason}
          end;
    {error, Reason} -> {error, Reason}
  end.

stats(Server) ->
	Server ! {self(), stats},
	receive
		Result -> Result
	end.
stats_(Sock) ->
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
		{PId, put, K, V} -> PId ! put_(Socket, K, V),
												wait_for_clients(Socket);
		{PId, del, K} -> PId ! del_(Socket, K),
										 wait_for_clients(Socket);
		{PId, get, K} -> PId ! get_(Socket, K),
										 wait_for_clients(Socket);
		{PId, take, K} -> PId ! take_(Socket, K),
											wait_for_clients(Socket);
		{PId, stats} -> PId ! stats_(Socket),
										wait_for_clients(Socket);
    close -> gen_tcp:close(Socket);
		_ -> wait_for_clients(Socket)
	end.

start() ->
	case gen_tcp:connect("localhost", 889, [binary, {active, false}, {packet, raw}]) of
		{ok, Socket} -> {ok, spawn(?MODULE, wait_for_clients, [Socket])};
		Error-> Error
	end.

close(Server) -> Server ! close.