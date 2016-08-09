%%%-------------------------------------------------------------------
%%% File    : udp.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Udp Protocol Support
%%%
%%% Created :  6 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(udp).

-import(checksum,[checksum/1]).
-import(packet_check,[check_packet/4, compute_checksum/5]).
-export([start/1,init/1, init_reader/0, init_writer/1, recv/3, send/4, usr_open/3]).

-include("ip.hrl").
%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Ip_Addr) ->
    init(Ip_Addr).

recv(Src_Ip, Dst_Ip, Data) ->
    udp_reader ! {recv, Src_Ip, Dst_Ip, Data}.

send(Dst_Ip, Dst_Port, Src_Port, Data) ->
    udp_writer ! {send, Dst_Ip, Dst_Port, Src_Port, Data}.

usr_open(Lc_Port, Dst_Ip, Dst_Port) -> %% This will send incoming packets to Lc_Port from Dst_Ip, Dst_Port to the calling process as {udp, {Lc_Ip, Lc_Port, Dst_Ip, Dst_Port}, Data}
	udp_reader ! {open, Lc_Port, Dst_Ip, Dst_Port, self()}.

%%%%%%%%%%%%%% Reader and Writer Loops %%%%%%%%%%%%%%

% We need the ip here to be able to compute the checksum
% as it includes a pseudo ip header. This should be fixed if
% we want to support more than one ip
init(Ip_Addr) ->
    R = spawn(udp, init_reader, []),
    register(udp_reader, R),
    W = spawn(udp, init_writer, [Ip_Addr]),
    register(udp_writer, W).

init_reader() ->
    reader_loop([]).

init_writer(Ip_Addr) ->
    writer_loop(Ip_Addr).

reader_loop(Conns) ->
	receive
		{recv, Src_Ip, Dst_Ip, Packet} ->
			case catch decode(Src_Ip, Dst_Ip, Packet) of
				{ok, Src_Ip, Dst_Ip, Src_Port, Dst_Port, Data} ->
					case lists:keyfind({Dst_Port, Src_Ip, Src_Port},1,Conns) of
						{_, P} ->
							P ! {udp, {Dst_Ip, Dst_Port, Src_Ip, Src_Port}, Data};
						false -> %% Packet received for which no one is listening. Ignore
							ok
					end;
				{error, Error} ->
					{error, Error}
			end,
			reader_loop(Conns);
		{open, Lc_Port, Dst_Ip, Dst_Port, From} ->
			N_Conn = {{Lc_Port, Dst_Ip, Dst_Port}, From},
			reader_loop( [N_Conn | Conns -- [N_Conn]])
	end.

writer_loop(Ip_Addr) ->
    receive 
	{send, Dst_Ip, Dst_Port, Src_Port, Data} ->
	    send_packet(Dst_Ip, Dst_Port, Ip_Addr, Src_Port, Data)
    end,
    writer_loop(Ip_Addr).

%%%%%%%%%%%%%% Reader Help Functions %%%%%%%%%%%%%%%%%%

decode(Src_Ip, Dst_Ip, Packet) when is_binary(Packet) ->
    case check_packet(Src_Ip, Dst_Ip, ?IP_PROTO_UDP, Packet) of
	ok ->
	    <<Src_Port:16/big-integer,
	     Dst_Port:16/big-integer,
	     _Len:16/big-integer,
	     _:16/big-integer,
	     Data/binary>> = Packet,
	    {ok, Src_Ip, Dst_Ip, Src_Port, Dst_Port, Data}; % Should check length?
	{error, Error} ->
	    {error, Error}
    end.

%%%%%%%%%%%%%%%%% Writer Help Functions %%%%%%%%%%%%%%%%%%%

send_packet(Dst_Ip, DPort, Src_Ip, SPort, Data) ->
    Len = size(Data) + 8,
    Pre_Checksum = <<SPort:16/big-integer,
		    DPort:16/big-integer,
		    Len:16/big-integer>>,
    Packet = <<Pre_Checksum/binary,
	      0:16/big-integer,
	      Data/binary>>,
    Checksum = compute_checksum(Src_Ip, Dst_Ip, ?IP_PROTO_UDP, Packet, size(Packet)),
    Checksum_Packet = <<Pre_Checksum/binary, 
		      Checksum:16/big-integer, 
		      Data/binary>>,
    ip:send(Checksum_Packet, size(Checksum_Packet), udp, Dst_Ip).
