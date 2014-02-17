-module(icinga).

-behaviour(gen_server).

-export([start_link/0]).
-export([submit/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type return_code() :: ok | warning | critical.
-export_type([return_code/0]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec submit(return_code(), iodata(), iodata()) -> ok.
submit(ReturnCode, ServiceDescription, PluginOutput) ->
    gen_server:cast(?SERVER, {submit, ReturnCode, ServiceDescription, PluginOutput}).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast({submit, ReturnCode, ServiceDescription, PluginOutput}, State) ->
    case icinga_cfg:server_hostname() of
        undefined ->
            ok;
        IcingaHostname ->
            IcingaPort = icinga_cfg:server_port(),
            ClientHost = icinga_cfg:client_hostname(),
            IcingaPassword = icinga_cfg:server_password(),
            Timeout = icinga_cfg:server_timeout(),
            ServiceDescriptionBin = iolist_to_binary(ServiceDescription),
            PluginOutputBin = iolist_to_binary(PluginOutput),
            ok = send(IcingaHostname, IcingaPort, IcingaPassword, ReturnCode, ClientHost, ServiceDescriptionBin, PluginOutputBin, Timeout)
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send(IcingaHostname, IcingaPort, IcingaPassword, ReturnCode, ClientHost, ServiceDescriptionBin, PluginOutputBin, Timeout) ->
    Fun =
        fun() ->
                case send_nsca:send(IcingaHostname, IcingaPort, IcingaPassword, ReturnCode, ClientHost, ServiceDescriptionBin, PluginOutputBin, Timeout) of
                    ok ->
                        lager:info("Icinga: ~s:~B - ~s, ~s, ~s", [IcingaHostname, IcingaPort, ReturnCode, ServiceDescriptionBin, PluginOutputBin]);
                    {error, Error} ->
                        lager:error("Icinga: ~p", [Error])
                end
        end,
    _Pid = spawn(Fun),
    ok.
