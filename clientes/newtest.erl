-module(newtest).
-export([test/2, client/3, spawner/4]).
-import(cli_bin, [put/3, del/2, get/2, take/2, stats/1, start/0, close/1]).
%-import(cli_text, [put/3, del/2, get/2, take/2, stats/1, start/0, close/1]).

recv(N) ->
	case N of
		0 -> ok;
		N -> 	receive
						ok -> recv(N-1);
						_ -> recv(N)
				 	end
	end.

client(Port, N, FatherPid) ->
	cli_bin:put(Port, N, N),
	cli_bin:get(Port, N),
	% cli_bin:del(Port, N), %Comentado para probar el deallocate
	FatherPid ! ok.

% client(Port, N, FatherPid) ->
% 	cli_text:put(Port, integer_to_list(N), integer_to_list(N)),
% 	cli_text:get(Port, integer_to_list(N)),
% 	cli_text:del(Port, integer_to_list(N)),
% 	cli_text:close(Port),
% 	FatherPid ! ok.
% change cli_bin to cli_text

spawner(RemainingClients, TotalClients, Port, FatherPid) ->
	case RemainingClients of			
		0 -> recv(TotalClients),
				 cli_bin:close(Port),
				 FatherPid ! ok;
		RemainingClients -> spawn(?MODULE, client, [Port, RemainingClients, self()]),
												spawner(RemainingClients-1, TotalClients, Port, FatherPid)
	end. 

test_aux(RemainingPorts, TotalPorts, ClientsPerPort) ->
	case RemainingPorts of
		0 -> recv(TotalPorts),
				 case start() of
						{ok, Port} -> cli_bin:stats(Port);
						Error -> Error
				 end;
		RemainingPorts -> case start() of
												{ok, Port} -> spawn(?MODULE, spawner, [ClientsPerPort, ClientsPerPort, Port, self()]),
																			test_aux(RemainingPorts-1, TotalPorts, ClientsPerPort);
												Error -> Error
				 end
	end.

test(Ports, ClientsPerPort) ->
	test_aux(Ports, Ports, ClientsPerPort).