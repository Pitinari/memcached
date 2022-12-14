-module(test).
-export(spawner/3, test/1)
-import(cli_bin, [put/3, del/2, get/2, take/2, stats/1, start/0, close/1]).
-import(cli_text, [put/3, del/2, get/2, take/2, stats/1, start/0, close/1]).

spawner(Port, N, FunctionIdx, Module, FatherPid) ->
	case N of
		0 ->  FatherPid ! ok;
		N ->  case FunctionIdx of
						1 -> spawn(Module, put, [Port, N, N]),
								spawner(Port, N - 1, FunctionIdx);
						2 -> spawn(Module, del, [Port, N]),
								spawner(Port, N - 1, FunctionIdx);
						3 -> spawn(Module, get, [Port, N]),
								spawner(Port, N - 1, FunctionIdx);
						4 -> spawn(Module, take, [Port, N]),
								spawner(Port, N - 1, FunctionIdx);
						5 -> spawn(Module, stats, [Port]),
								spawner(Port, N - 1, FunctionIdx)
					end
	end.


test(N, Module) ->
	case start() of
		{ok, Port} -> spawner(Port, N, 1, Module, getpid()),
									receive
										ok -> ok
									end,
									spawner(Port, N, 3, Module, getpid()),
									receive
										ok -> ok
									end,
									spawner(Port, N, 2, Module, getpid()),
									receive
										ok -> ok
									end,
									Module:stats(Port);
		Error -> Error
	end. 