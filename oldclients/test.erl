-module(test).
-export([test/1, client/3]).
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
	cli_bin:close(Port),
	FatherPid ! ok.

% client(Port, N, FatherPid) ->
% 	cli_text:put(Port, integer_to_list(N), integer_to_list(N)),
% 	cli_text:get(Port, integer_to_list(N)),
% 	cli_text:del(Port, integer_to_list(N)),
% 	cli_text:close(Port),
% 	FatherPid ! ok.
% change cli_bin to cli_text

test(N) ->
	test_aux(N, N).

test_aux(N, M) ->
	case N of			
		0 -> recv(M),
				 case start() of
					{ok, Port} -> cli_bin:stats(Port);
					Error -> Error
				 end;
		N -> case start() of
					{ok, Port} -> spawn(test, client, [Port, N, self()]),
												test_aux(N-1, M);
					Error -> Error
				 end
	end. 