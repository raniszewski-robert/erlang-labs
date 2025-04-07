-module(pingpong).
-export([start/0, ping_loop/1, pong_loop/0, play/1, stop/0]).


start() ->
  register(ping, spawn(pingpong, ping_loop, [0])),
  register(pong, spawn(pingpong, pong_loop, [])).

ping_loop(S) ->

  receive
    stop -> ok;
    0 -> ping_loop(S);
    N ->
      NewS = S + N,
      timer:sleep(200),
      io:format("ping ~w, sum ~w~n", [N, NewS]),
      pong ! N-1,
      ping_loop(NewS)
  after
    20000 -> io:format("stopped ~w~n", [self()])
  end.

pong_loop() ->
  receive
    stop -> ok;
    0 -> pong_loop();
    N ->
      timer:sleep(200),
      io:format("pong ~w~n", [N]),
      ping ! N-1,
      pong_loop()
  after
    20000 -> io:format("stopped ~w~n", [self()])
  end.

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! N.

