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
    meck:new(icinga_os),

    meck:expect(icinga_cfg, server_hostname     , fun() -> "localhost" end),
    meck:expect(icinga_cfg, send_ncsa_executable, fun() -> "/sbin/send_nsca" end),
    meck:expect(icinga_cfg, send_ncsa_config    , fun() -> "/etc/send_nsca.cfg" end),
    meck:expect(icinga_cfg, client_hostname     , fun() -> "dev-build" end),

    Cmd = "echo \"dev-build\tservice\t0\tcomplicated error msg\" | /sbin/send_nsca -c /etc/send_nsca.cfg -H localhost",
    meck:expect(icinga_os, cmd, fun(X) -> X = Cmd, ok end),

    {noreply, _State} = icinga:handle_cast({submit, ok, "service", "complicated\nerror\nmsg"}, fake_state),
    
    meck:unload(icinga_os),
    meck:unload(icinga_cfg).
