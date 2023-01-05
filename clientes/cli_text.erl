-module(cli_text).
-export([put/3, del/2, get/2, take/2, stats/1, start/0, close/1, wait_for_clients/1]).

recv(Socket, Remainder) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} -> case lists:suffix("\n", Packet) of
											true -> {ok, lists:concat([Remainder, Packet])};
											false -> recv(Socket, lists:concat([Remainder, Packet]))
										end;
		Error -> Error
	end.


put(Server, K, V) ->
	Server ! {self(), put, K, V},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
put_(Socket, K, V)->
 case gen_tcp:send(Socket, "PUT " ++ K ++ " " ++ V ++ "\n") of
		ok -> case recv(Socket, []) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
	end.

del(Server, K) ->
	Server ! {self(), del, K},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
del_(Socket, K) ->
	case gen_tcp:send(Socket, "DEL " ++ K ++ "\n") of
		ok -> case recv(Socket, []) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
	end.

get(Server, K) ->
	Server ! {self(), get, K},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
get_(Socket, K) ->
	case gen_tcp:send(Socket, "GET " ++ K ++ "\n") of
		ok -> case recv(Socket, []) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
	end.

take(Server, K) ->
	Server ! {self(), take, K},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
take_(Socket, K) ->
	case gen_tcp:send(Socket, "TAKE " ++ K ++ "\n") of
		ok -> case recv(Socket, []) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
	end.

stats(Server) ->
	Server ! {self(), stats},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason} 
	end.
stats_(Socket) ->
	case gen_tcp:send(Socket, "STATS\n") of
		ok -> case recv(Socket, []) of
						{ok, Packet} -> {ok, Packet};
						Error -> Error
					end;
		Error -> Error
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
	case gen_tcp:connect("localhost", 888, [{active, false}, {packet, raw}]) of
		{ok, Socket} -> {ok, spawn(?MODULE, wait_for_clients, [Socket])};
		Error-> Error
	end.

close(Server) -> Server ! close.
