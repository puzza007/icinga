-module(icinga_cfg).

-export([ server_hostname/0
        , send_ncsa_executable/0
        , send_ncsa_config/0
        , client_hostname/0
        ]).


server_hostname() ->
    {ok, IcingaHostname} = application:get_env(icinga, server_hostname),
    IcingaHostname.

send_ncsa_executable() ->
    {ok, SendNcsa} = application:get_env(icinga, send_ncsa_executable),
    SendNcsa.

send_ncsa_config() ->
    {ok, SendNcsaCfg} = application:get_env(icinga, send_ncsa_config),
    SendNcsaCfg.

client_hostname() ->
    {ok, Hostname} = application:get_env(icinga, client_hostname),
    Hostname.
