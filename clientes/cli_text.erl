-module(memcached).
-export([put/3, del/2, get/2, take/2, stats/1]).

put(Sock, K, V) ->
  ok = gen_tcp:send(Sock, "PUT " ++ K ++ " " ++ V ++ "~n"), % PUT = 11
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, TamañoDeStats) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason]);
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

del(Sock, K) ->
  ok = gen_tcp:send(Sock, 12), % DEL = 12
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, TamañoDeStats) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason]);
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

get(Sock, K) ->
  ok = gen_tcp:send(Sock, 13), % GET = 13
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, TamañoDeStats) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason]);
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

take(Sock, K) ->
  ok = gen_tcp:send(Sock, 14), % TAKE = 14
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, TamañoDeStats) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason]);
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.

stats(Sock) ->
  ok = gen_tcp:send(Sock, 21), % STATS = 21
  case gen_tcp:recv(Sock, 1) of
    101 -> case gen_tcp:recv(Sock, TamañoDeStats) of
             {ok, Packet} -> io:fwrite("~p~n", [Packet]);
             {error, Reason} -> io:fwrite("error en recv: ~p~n", [Reason]);
           end;
    Cod -> io:fwrite("error en pedido, devolvió ~p ~n", [Cod])
  end,
  ok.


%	}
%	/* Ver respuesta */
%	{
%		int cod = 0;
%		readn(fd, &cod, 1);
%		int len;
%		void *buf;
%		recv_var(fd, &len, &buf);
%		writen(1, buf, len);
%		writen(1, "\n", 1);
%		free(buf);
%
%		fprintf(stderr, "\nOK\n");
%	}
%}

%conectar una sola vez
%una solucion es tener un hilo en init que responde con el socket y llamarlo en las funciones
