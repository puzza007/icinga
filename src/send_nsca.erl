-module(send_nsca).

-export([send/8]).

-define(PACKET_VERSION, 3).

connect_and_get_keys(Host, Port, Timeout) ->
    Opts = [binary, {nodelay, true}, {active, false}],
    case gen_tcp:connect(Host, Port, Opts, Timeout) of
        {ok, Sock} ->
            case gen_tcp:recv(Sock, 132, Timeout) of
                {ok, <<XorKey:128/binary, Timestring/binary>>} ->
                    {ok, Sock, XorKey, Timestring};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

pack(Crc, Timestring, ReturnCode, Host, Service, Status) when
      is_integer(Crc) andalso
      is_binary(Timestring) andalso
      is_integer(ReturnCode) andalso
      is_binary(Host) andalso
      is_binary(Service) andalso
      is_binary(Status) ->
    HostPadded = pad_to(Host, 64),
    ServicePadded = pad_to(Service, 128),
    StatusPadded = pad_to(Status, 512),
    <<?PACKET_VERSION:16/integer, % n
      0:16,                       % xx
      Crc:32/integer,             % N
      Timestring:4/binary,        % a4
      ReturnCode:16/integer,      % n
      HostPadded/binary,          % a64
      ServicePadded/binary,       % a128
      StatusPadded/binary,        % a512
      0:16>>.                     % xx

-spec send(inet:hostname(), pos_integer(), binary(), icinga:return_code(), inet:hostname(),
           binary(), binary(), pos_integer()) ->
                  ok | {error, any()}.
send(IcingaHost, Port, Password, ReturnCode, Host, Service, Status, Timeout) when
      is_list(IcingaHost) andalso
      is_integer(Port) andalso
      is_binary(Password) andalso
      is_atom(ReturnCode) andalso
      is_list(Host) andalso
      is_binary(Service) andalso
      is_binary(Status) ->
    case return_code(ReturnCode) of
        {error, _} = Error ->
            Error;
        ReturnCodeInt ->
            case connect_and_get_keys(IcingaHost, Port, Timeout) of
                {ok, Sock, XorKey, Timestring} ->
                    HostBin = list_to_binary(Host),
                    StringToSendWithoutCrc = pack(0, Timestring, ReturnCodeInt, HostBin, Service, Status),
                    Crc = erlang:crc32(StringToSendWithoutCrc),
                    StringToSendWithCrc = pack(Crc, Timestring, ReturnCodeInt, HostBin, Service, Status),
                    EncryptedStringToSend = xor_with_key(XorKey, StringToSendWithCrc),
                    EncryptedStringToSend2 = xor_with_key(Password, EncryptedStringToSend),
                    Result = gen_tcp:send(Sock, EncryptedStringToSend2),
                    ok = gen_tcp:close(Sock),
                    Result;
                {error, _} = Error ->
                    Error
            end
    end.

xor_with_key(Key, Bin) when is_binary(Key) andalso is_binary(Bin) ->
    %% Repeat Key until it's the exact same length as Bin
    SizeB = byte_size(Bin),
    N = (SizeB div byte_size(Key)) + 1,
    K2 = lists:duplicate(N, Key),
    K3 = list_to_binary(K2),
    K4 = <<K3:SizeB/binary>>,
    binary_xor(K4, Bin).

binary_xor(A, B) when is_bitstring(A) andalso
                      is_bitstring(B) andalso
                      bit_size(A) =:= bit_size(B) ->
    BitSize = bit_size(A),
    <<IA:BitSize>> = A,
    <<IB:BitSize>> = B,
    I = IA bxor IB,
    <<I:BitSize>>.

pad_to(Bin, Len) ->
    Size = byte_size(Bin),
    Padding = max(0, (Len - Size) * 8),
    <<Bin/binary, 0:Padding>>.

return_code(ok) ->
    0;
return_code(warning) ->
    1;
return_code(critical) ->
    2;
return_code(_Unknown) ->
    {error, unknown_return_code}.
