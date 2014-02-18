-module(icinga).


-export([submit_async/3]).
-export([submit_sync/3]).

-type return_code() :: ok | warning | critical.
-export_type([return_code/0]).

-spec submit_async(return_code(), iodata(), iodata()) -> ok.
submit_async(ReturnCode, ServiceDescription, PluginOutput) ->
    submit(ReturnCode, ServiceDescription, PluginOutput, fun send_async/8).

-spec submit_sync(return_code(), iodata(), iodata()) -> ok | {error, any()}.
submit_sync(ReturnCode, ServiceDescription, PluginOutput) ->
    submit(ReturnCode, ServiceDescription, PluginOutput, fun send_sync/8).

submit(ReturnCode, ServiceDescription, PluginOutput, SendFun)
  when ReturnCode =:= ok orelse
       ReturnCode =:= warning orelse
       ReturnCode =:= critical ->
    IcingaHostname = icinga_cfg:server_hostname(),
    IcingaPort = icinga_cfg:server_port(),
    ClientHost = icinga_cfg:client_hostname(),
    IcingaPassword = icinga_cfg:server_password(),
    Timeout = icinga_cfg:server_timeout(),
    ServiceDescriptionBin = iolist_to_binary(ServiceDescription),
    PluginOutputBin = iolist_to_binary(PluginOutput),
    SendFun(IcingaHostname, IcingaPort, IcingaPassword, ReturnCode, ClientHost, ServiceDescriptionBin, PluginOutputBin, Timeout).

send_async(IcingaHostname, IcingaPort, IcingaPassword, ReturnCode, ClientHost, ServiceDescriptionBin, PluginOutputBin, Timeout) ->
    Fun =
        fun() ->
                case send_nsca:send(IcingaHostname, IcingaPort, IcingaPassword, ReturnCode, ClientHost, ServiceDescriptionBin, PluginOutputBin, Timeout) of
                    ok ->
                        Args = [IcingaHostname, IcingaPort, ReturnCode, ServiceDescriptionBin, PluginOutputBin],
                        lager:info("Icinga: ~s:~B - ~s, ~s, ~s", Args);
                    {error, Error} ->
                        Args = [Error, IcingaHostname, IcingaPort, ReturnCode, ServiceDescriptionBin, PluginOutputBin],
                        lager:error("Icinga: ~p from: ~s:~B - ~s, ~s, ~s", Args)
                end
        end,
    _Pid = spawn(Fun),
    ok.

send_sync(IcingaHostname, IcingaPort, IcingaPassword, ReturnCode, ClientHost, ServiceDescriptionBin, PluginOutputBin, Timeout) ->
    case send_nsca:send(IcingaHostname, IcingaPort, IcingaPassword, ReturnCode, ClientHost, ServiceDescriptionBin, PluginOutputBin, Timeout) of
        ok ->
            Args = [IcingaHostname, IcingaPort, ReturnCode, ServiceDescriptionBin, PluginOutputBin],
            lager:info("Icinga: ~s:~B - ~s, ~s, ~s", Args),
            ok;
        {error, Error} = E ->
            lager:error("Icinga: ~p", [Error]),
            E
    end.
