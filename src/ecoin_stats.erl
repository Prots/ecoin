-module(ecoin_stats).

-export([create/1,
         sent/3,
         received/3]).

-include("ecoin.hrl").

%% @doc Create all statistics entries we want for a new peer.
%%      Since there are multiple processes invloved we
%%      key everything on the socket.
-spec create(socket()) -> ok.
create(Socket) ->
    exometer:new([messages, in,  num,  Socket], counter),
    exometer:new([messages, in,  size, Socket], counter),
    exometer:new([messages, out, num,  Socket], counter),
    exometer:new([messages, out, size, Socket], counter).

%% @doc Log sent messages type and size
-spec sent(socket(), command(), uinteger()) -> ok.
sent(Socket, Command, Length) ->
    on_message(Socket, out, Command, Length).

%% @doc Log received messages type and size
-spec received(socket(), command(), uinteger()) -> ok.
received(Socket, Command, Length) ->
    on_message(Socket, in, Command, Length).

%% @doc Helper for sent and received
-spec on_message(socket(), in | out, command(), uinteger()) -> ok.
on_message(Socket, Dir, Command, Length) ->
    exometer:update([messages, Dir, num,  Socket], 1),
    exometer:update([messages, Dir, size, Socket], 24 + Length),
    exometer:update([messages, Dir, Command], 1).
