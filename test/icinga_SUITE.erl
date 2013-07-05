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
    meck:new(ubic_application),
    meck:new(ubic_os),
    {ok, Hostname} = inet:gethostname(),

    meck:expect(ubic_application, get_env, fun(icinga, server_hostname) -> {ok, "localhost"};
                                              (icinga, send_ncsa_executable) -> {ok, "/sbin/send_ncsa"};
                                              (icinga, send_ncsa_config) -> {ok, "/etc/send_ncsa.cfg"}
                                           end),
    Cmd = "echo \"" ++ Hostname ++ "\tservice\t0\tcomplicated error msg\" | /sbin/send_nsca -c /etc/send_nsca.cfg -H localhost",
    meck:expect(ubic_os, cmd, fun(X) -> X end),

    {noreply, _State} = icinga:handle_cast({submit, ok, "service", "complicated\nerror\nmsg"}, fake_state),
    
    meck:unload(ubic_os),
    meck:unload(ubic_application).
