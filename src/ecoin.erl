-module(ecoin).

-export([start/0]).

start() ->
    application:ensure_all_started(ecoin),
    application:start(ecoin).
