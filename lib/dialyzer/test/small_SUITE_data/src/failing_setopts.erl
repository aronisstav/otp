-module(failing_setopts).

-export([test/1]).

test(Sock) ->
    inet:setopts(Sock, [{active, true}, {packet, raw}, binary]).
