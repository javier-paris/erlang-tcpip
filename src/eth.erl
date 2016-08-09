%%%-------------------------------------------------------------------
%%% File    : eth.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Ethernet Link Layer
%%%
%%% Created :  2 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(eth).

-export([start/1,init_reader/1, init_writer/1, send/3, recv/1, get_mtu/0]).
-include("eth.hrl").

%%% API %%%%%%%%%
start(Mac_Addr) ->
    init(Mac_Addr).

send(Packet, Protocol, Mac_Addr) ->
    catch eth_writer ! {send, Packet, Protocol, Mac_Addr}.

recv(Packet) ->
    catch eth_reader ! {recv, Packet}.

get_mtu() ->
    catch eth_writer ! {get_mtu, self()},
    receive
	{mtu, MTU} ->
	    {mtu, MTU}
    end.

%%% Reader and Writer Loops %%%%%%%

init(Mac_Addr) ->
    spawn(eth, init_reader, [Mac_Addr]),
    spawn(eth, init_writer, [Mac_Addr]).

init_reader(Mac_Addr) ->
    register(eth_reader, self()),
    reader_loop(Mac_Addr).

init_writer(Mac_Addr) ->
    register(eth_writer, self()),
    writer_loop(Mac_Addr).

writer_loop(Mac_Addr) ->
    receive
	{send, Packet, Protocol, Dst_Mac_Addr} ->
	    send_packet(Packet, Protocol, Dst_Mac_Addr, Mac_Addr);
	{get_mtu, From} ->
	    From ! eth_port:get_mtu()
    end,
    writer_loop(Mac_Addr).

reader_loop(Mac_Addr) ->
    receive
		{recv, Packet} ->
			case catch decode(Packet,Mac_Addr) of
				{ok, Protocol, Data} ->
					Protocol:recv(Data);
				{error, Error} ->
					{error, Error}; % Should probably implement an optional logging for decoding errors
				_ ->
					{error, unknown}
			end
    end,
    reader_loop(Mac_Addr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Writer help Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_packet(Packet, Protocol, Dst_Mac, Src_Mac) ->
    Eth_Protocol = protocol(Protocol),
    eth_port:send([<<Dst_Mac:48/big-integer, Src_Mac:48/big-integer,
		   Eth_Protocol:16/big-integer>>, 
		   Packet]).
	

%% Ethernet protocol constant to atom, and viceversa
%%  atom to constant is used to encode outgoing packets, 
%%  constant to atom is used to decide which module will receive the decoded packet

protocol(ip) -> ?ETH_IP;
protocol(arp) -> ?ETH_ARP;
protocol(?ETH_IP) -> ip;
protocol(?ETH_ARP) -> arp.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reader Help Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Packet, Mac) when is_binary(Packet) ->
    case Packet of
	<<Mac:48/big-integer, _Src:48/big-integer, 
	 Protocol:16/big-integer,
	 Data/binary>> -> % For us
	    {ok, protocol(Protocol), Data};
	<<?ETH_BROAD:48/big-integer, _Src:48/big-integer,
	 Protocol:16/big-integer, 
	 Data/binary>> -> % Broadcast
	    {ok, protocol(Protocol), Data};
	_ ->
	    {error, not_for_us}
    end.
