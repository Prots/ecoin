-module(ecoin).

-export([start/0]).

start() ->
    application:start(crypto),
    application:ensure_all_started(ecoin),
    application:start(ecoin).
