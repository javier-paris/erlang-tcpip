%%%-------------------------------------------------------------------
%%% File    : icmp.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Icmp Protocol implementation
%%%
%%% Created :  4 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
%%%
%%%
%%% erlang-tcpip, Copyright (C) 2004 Javier Paris
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------

-module(icmp).

-import(checksum,[checksum/1]).
-export([start/0,init_reader/0, init_writer/0, recv/2, send/2]).

-define(ICMP_ECHO_REPLY, 0).
-define(ICMP_DESTINATION_UNREACHABLE, 3).
-define(ICMP_SOURCE_QUENCH, 4).
-define(ICMP_REDIRECT, 5).
-define(ICMP_ECHO_REQUEST, 8).
-define(ICMP_TIME_EXCEEDED, 11).
-define(ICMP_PARAMETER_PROBLEM, 12).
-define(ICMP_TIMESTAMP_REQUEST, 13).
-define(ICMP_TIMESTAMP_REPLY, 14).
-define(ICMP_INFORMATION_REQUEST, 15).
-define(ICMP_INFORMATION_REPLY, 16).

-define(ICMP_DST_UNR_NET, 0).
-define(ICMP_DST_UNR_HOST, 1).
-define(ICMP_DST_UNR_PROTOCOL, 2).
-define(ICMP_DST_UNR_PORT, 3).
-define(ICMP_DST_UNR_FRAGMENTATION, 4).
-define(ICMP_DST_UNR_SOURCE_ROUTE, 5).


start() ->
    init().

init() ->
    spawn(icmp, init_writer, []),
    spawn(icmp, init_reader, []).

init_reader() ->
    register(icmp_reader, self()),
    reader_loop().

init_writer() ->
    register(icmp_writer, self()),
    writer_loop().

recv(Src_Ip, Data) ->
    icmp_reader ! {recv, Src_Ip, Data}. 

send(Type, Data) ->
    icmp_writer ! {send, Type, Data}.

reader_loop() ->
    receive
	{recv, Src_Ip, Data} ->
	    decode(Src_Ip, Data)
    end,
    reader_loop().

writer_loop() ->
    receive
	{send, echo, {Type, Dst_Ip, Identifier, Sequence, Data}} ->
	    Packet = create_echo(Type, Identifier, Sequence, Data),
	    ip:send(Packet, size(Packet), icmp, Dst_Ip);
	_ ->
	    ok
    end,
    writer_loop().

decode(Src_Ip, Packet) ->
    <<Type:8/integer, 
      Code:8/integer, 
     _:2/binary,
     Remainder/binary>> = Packet, 
    case Type of
	?ICMP_ECHO_REQUEST ->
	    case decode_echo(Remainder) of
		{ok, Identifier, Sequence, Data} ->
		    send(echo, {reply, Src_Ip, Identifier, 
				Sequence, Data});
		_ ->
		    {error, bad_echo_request}
	    end;
	?ICMP_ECHO_REPLY ->
	    case decode_echo(Remainder) of
		{ok, _Identifier, Sequence, _Data} ->
		    io:format("Ping from ~w:~w~n",[Src_Ip, Sequence]);
		_ ->
		    {error, bad_echo_reply}
	    end;
	?ICMP_DESTINATION_UNREACHABLE ->
	    case Code of
		?ICMP_DST_UNR_FRAGMENTATION -> % For PMTU
		    <<_:2/binary, MTU:16/big-integer,
		     IP_Packet/binary>> = Remainder,
		    ip:change_mtu(IP_Packet, MTU);
		_ ->                           % Just notify the error
		    <<_:4/binary, IP_Packet/binary>> = Remainder,
		    ip:dst_unreachable(IP_Packet)
	    end;
	_ ->
	    {error, notsupported}
    end.
     
decode_echo(Packet) ->
    <<Identifier:16/big-integer, Sequence:16/big-integer, 
     Data/binary>> = Packet,
    {ok, Identifier, Sequence, Data}.

create_echo(Type, Identifier, Sequence, Data) ->
    Num_Type = type(Type),
    Packet = <<Num_Type:8/integer, 0:8/integer, 0:16/big-integer,
	      Identifier:16/big-integer, Sequence:16/big-integer,
	      Data/binary>>,
    Checksum = checksum(Packet),
    <<Pre_Checksum:2/binary,_:2/binary,
     Post_Checksum/binary>> = Packet,
    <<Pre_Checksum/binary, Checksum:16/integer,
     Post_Checksum/binary>>.

type(request) -> ?ICMP_ECHO_REQUEST;
type(reply) -> ?ICMP_ECHO_REPLY.
    
