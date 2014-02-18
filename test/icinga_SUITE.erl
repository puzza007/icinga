-module(icinga_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [t_submit].

t_submit(_Config) ->
    meck:new(icinga_cfg),
    meck:new(send_nsca),

    meck:expect(icinga_cfg, server_hostname, fun() -> "icserv" end),
    meck:expect(icinga_cfg, server_port, fun() -> 6666 end),
    meck:expect(icinga_cfg, client_hostname, fun() -> "dev-build" end),
    meck:expect(icinga_cfg, server_password, fun() -> "pass" end),
    meck:expect(icinga_cfg, server_timeout, fun() -> 7500 end),

    meck:expect(send_nsca, send, fun
                                     ("icserv", 6666, "pass", ok, "dev-build", <<"as_desc">>, <<"as_pl_output">>, 7500) -> ok;
                                     ("icserv", 6666, "pass", ok, "dev-build", <<"s_desc">>, <<"s_pl_output">>, 7500) -> ok
                                 end),

    ok = icinga:submit_async(ok, ["as_", <<"desc">>], [<<"as">>, "_pl_output"]),
    ok = icinga:submit_sync(ok, ["s_", <<"desc">>], [<<"s">>, "_pl_output"]),

    meck:unload(send_nsca),
    meck:unload(icinga_cfg).
