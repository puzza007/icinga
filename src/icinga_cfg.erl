-module(icinga_cfg).

-export([ server_hostname/0
        , server_port/0
        , server_password/0
        , client_hostname/0
        ]).


-spec server_hostname() -> inet:hostname().
server_hostname() ->
    {ok, IcingaHostname} = application:get_env(icinga, server_hostname),
    IcingaHostname.

-spec server_port() -> pos_integer().
server_port() ->
    application:get_env(icinga, server_port, 5667).

-spec server_password() -> binary().
server_password() ->
    {ok, Password} = application:get_env(icinga, server_password),
    list_to_binary(Password).

-spec client_hostname() -> inet:hostname().
client_hostname() ->
    {ok, Hostname} = application:get_env(icinga, client_hostname),
    Hostname.
