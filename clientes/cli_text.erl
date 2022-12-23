-module(cli_text).
-export([put/2, del/1, get/1, take/1, stats/0, start/0, start_aux/0, close/1]).

put(K, V) ->
	server ! {self(), put, K, V},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
put(Socket, K, V)->
 case gen_tcp:send(Socket, "PUT " ++ K ++ " " ++ V ++ "\n") of
		ok -> case gen_tcp:recv(Socket, 0) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
	end.

del(K) ->
	server ! {self(), del, K},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
del(Socket, K) ->
	case gen_tcp:send(Socket, "DEL " ++ K ++ "\n") of
		ok -> case gen_tcp:recv(Socket, 0) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
	end.

get(K) ->
	server ! {self(), get, K},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
get(Socket, K) ->
	case gen_tcp:send(Socket, "GET " ++ K ++ "\n") of
		ok -> case gen_tcp:recv(Socket, 0) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
	end.

take(K) ->
	server ! {self(), take, K},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
take(Socket, K) ->
	case gen_tcp:send(Socket, "TAKE " ++ K ++ "\n") of
		ok -> case gen_tcp:recv(Socket, 0) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
	end.

stats() ->
	server ! {self(), stats},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
stats(Socket) ->
	case gen_tcp:send(Socket, "STATS\n") of
		ok -> case gen_tcp:recv(Socket, 0) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
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
	case gen_tcp:connect("localhost", 888, [{active, false}, {packet, raw}]) of
		{ok, Socket} -> register(server, self()),
										wait_for_clients(Socket);
		Error-> Error
	end.
start() ->
	spawn(cli_text, start_aux, []).	

close(Socket) -> gen_tcp:close(Socket).
