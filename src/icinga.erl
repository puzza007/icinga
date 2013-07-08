-module(icinga).

-behaviour(gen_server).

-export([start_link/0]).
-export([submit/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-type return_code() :: ok | warning | critical.

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec submit(return_code(), iolist(), iolist()) -> ok.
submit(ReturnCode, ServiceDescription, PluginOutput) ->
    gen_server:cast(?SERVER, {submit, ReturnCode, ServiceDescription, PluginOutput}).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({submit, ReturnCode, ServiceDescription, PluginOutput}, State) ->
    {ok, IcingaHostname} = ubic_application:get_env(icinga, server_hostname),
    {ok, SendNcsa} = ubic_application:get_env(icinga, send_ncsa_executable),
    {ok, SendNcsaCfg} = ubic_application:get_env(icinga, send_ncsa_config),
    case IcingaHostname of
        undefined ->
            ok;
        _ ->
            Msg = format(ReturnCode, ServiceDescription, PluginOutput),
            Cmd = lists:flatten(
                    io_lib:format("echo \"~s\" | ~s -c ~s -H ~s",
                                  [Msg, SendNcsa, SendNcsaCfg, IcingaHostname])
                   ),
            Res = ubic_os:cmd(Cmd),
            lager:info("Icinga command: ~s", [Cmd]),
            lager:info("Icinga result: ~s", [Res])
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% <host_name>[tab]<svc_description>[tab]<return_code>[tab]<plugin_output>[newline]

%% * <host_name>=short name of the host that the service is associated with (as defined in the host_name directive of the service definition)
%% * <svc_description>=description of the service (as defined in the service_description directive of the service definition)
%% * <return_code>=numeric return code (0,1,2,3 as explained here)
%% * <plugin_output>=output from host/service check

format(ReturnCode, ServiceDescription, PluginOutput) ->
    {ok, Hostname} = inet:gethostname(),
    I = iolist_to_binary(PluginOutput),
    I2 = binary:replace(I, <<"\n">>, <<" ">>, [global]),
    [Hostname, "\t", ServiceDescription, "\t", return_code(ReturnCode), "\t", I2].

return_code(ok) ->
    "0";
return_code(warning) ->
    "1";
return_code(critical) ->
    "2";
return_code(_) ->
    "3".
    
