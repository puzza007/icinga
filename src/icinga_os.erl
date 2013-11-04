%%% ---------------------------------------------------------------------------
%%% Wrapper for stdlib's os module, to ease mocking for tests.
%%% ---------------------------------------------------------------------------
-module(icinga_os).

-export([ cmd/1
        ]).

cmd(Cmd) ->
    os:cmd(Cmd).
