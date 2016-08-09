%%%-------------------------------------------------------------------
%%% File    : tcp.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp receive multiplexor
%%%
%%% Created : 11 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(tcp).

-export([start/0,recv/3,reader_init/0, new_mtu/3, dst_unr/3]).

-include("tcp_packet.hrl").

start() ->
    spawn(tcp, reader_init, []).

recv(Src_Ip, Dst_Ip, Data) ->
    catch tcp_reader ! {recv, Src_Ip, Dst_Ip, Data}.

new_mtu(Src_Ip, Dst_Ip, Data) ->
    catch tcp_reader ! {new_mtu, Src_Ip, Dst_Ip, Data}.

dst_unr(Src_Ip, Dst_Ip, Data) ->
    catch tcp_reader ! {dst_unr, Src_Ip, Dst_Ip, Data}.

reader_init() ->
    register(tcp_reader, self()),
    reader_loop().

reader_loop() ->
    receive
	{recv, Src_Ip, Dst_Ip, Data} ->
	    case catch tcp_packet:parse(Src_Ip, Dst_Ip, Data) of 
		{ok, Pkt} ->
		    demux_packet(Pkt);
		{error, Error} -> % Bad checksum
		    {error, Error};
		{'EXIT', _} ->
		    {error, badpacket}
	    end;
	{new_mtu, Src_Ip, Dst_Ip, {Data, MTU}} ->
	    <<Src_Port:16/big-integer, 
	      Dst_Port:16/big-integer, 
	      _/binary>> = Data,
	    case tcp_pool:get({Src_Ip, Src_Port, Dst_Ip, Dst_Port}) of
		{ok, Conn} ->
		    tcp_con:new_mtu(Conn, MTU);
		{error, Error} ->
		    {error, Error}
	    end;
	{dst_unr, Src_Ip, Dst_Ip, Data} ->
	    <<Src_Port:16/big-integer,
	      Dst_Port:16/big-integer,
	      _/binary>> = Data,
	    case tcp_pool:get({Src_Ip, Src_Port, Dst_Ip, Dst_Port}) of
		{ok, Conn} ->
		    tcp_con:dst_unr(Conn);
		{error, Error} ->
		    {error, Error}
	    end
    end,
    reader_loop().

demux_packet(Pkt) ->
    case tcp_pool:get({Pkt#pkt.dip, Pkt#pkt.dport, 
		       Pkt#pkt.sip, Pkt#pkt.sport}) of
	{ok, Conn} ->
	    tcp_con:recv(Conn, Pkt);
	{error, _} -> % Try to find a passive connection (Incoming syn?)
	    case tcp_pool:get({Pkt#pkt.dip, Pkt#pkt.dport}) of
		{ok, Conn} ->
		    tcp_con:recv(Conn, Pkt);
		{error, Error} ->
		    closed:recv(Pkt), % Send rst
		    {error, Error}
	    end
    end.
