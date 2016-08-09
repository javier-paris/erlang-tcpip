%%%-------------------------------------------------------------------
%%% File    : arp.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Arp Protocol Implementation
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

-module(arp).

-export([start/2,init_reader/2, init_writer/2, ip_queue_init/2, send/2, answer/2, solve/1, recv/1]).

-include("eth.hrl").

-define(ETH_LEN, 6).
-define(IP_LEN, 4).
-define(ARP_REQUEST, 1).
-define(ARP_REPLY, 2).

%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Ip, Mac) ->
    init(Ip, Mac).

% Send Packet to Ip Address. Every packet sent by ip passes through here
send(Packet, Ip_Addr) ->
    ip_queue ! {send, Packet, Ip_Addr}.

% Wake up any packet waiting for a given arp resolution
recv_reply(Ip_Addr, Mac_Addr) -> 
    ip_queue ! {arp, Ip_Addr, Mac_Addr}.

% Answer a arp request directed to us
answer(Ip_Addr,Mac) ->
    arp_writer ! {answer, Ip_Addr, Mac}.

% Make an Arp request to translate Ip_Addr
solve(Ip_Addr) ->
    ets:insert(arp_cache, {Ip_Addr, solving}),
    arp_writer ! {solve, Ip_Addr}.

% Received an arp packet
recv(Packet) ->    
    catch arp_reader ! Packet.


%%%%%%%%%% Queue, Reader and Writer Loops %%%%%%%%%%%%%

%% The arp cache is stored in an ets table. In the current implementation it is never purged, but could be implemented as a separate 
%% process that periodically looks at the entries

init(Ip, Mac) ->
    ets:new(arp_cache, [set, public, named_table]),
    spawn(arp, init_reader, [Ip, Mac]),
    spawn(arp, init_writer, [Ip, Mac]),
    spawn(arp, ip_queue_init, [Ip, Mac]). %% This process stores outgoing packets which have to wait for an arp reply
    
ip_queue_init(Ip, Mac) -> 
    ets:new(ip_queue,[bag, private, named_table]),  
    register(ip_queue, self()),
    ip_queue_loop(Ip, Mac).

init_reader(Ip, Mac) ->
    register(arp_reader, self()),
    reader_loop(Ip, Mac).

init_writer(Ip, Mac) ->
    register(arp_writer, self()),
    writer_loop(Ip, Mac).

ip_queue_loop(Ip, Mac) ->
    receive
		{send, Packet, Ip_Addr} -> %% Send a packet to Ip_Addr. Check if the address is in the arp cache, and enqueue the packet and send an arp request if it isn't
			case catch ets:lookup_element(arp_cache, Ip_Addr, 2) of
				{'EXIT', _} ->
					solve(Ip_Addr),
					queue(Ip_Addr, Packet);
				solving ->
					queue(Ip_Addr, Packet);
				Mac_Addr ->
					send_packet(Packet, Mac_Addr)
			end;
		{arp, Ip_Addr, Mac_Addr} ->
			Packets = dequeue(Ip_Addr),
			lists:foreach(fun(Packet) -> send_packet(Packet, Mac_Addr) end, Packets)
	end,
    ip_queue_loop(Ip, Mac).
    
reader_loop(Ip, Mac) ->
    receive
		Packet when is_binary(Packet) ->
			catch decode(Packet, Ip, Mac); %% The catch makes it ignore errors if the received packet is not a correct arp packet. Log?
		_ ->
			{error, not_binary} 
    end,
    reader_loop(Ip, Mac).

writer_loop(Ip, Mac) ->
    receive
	{solve, Ip_Dst} ->
	    send_arp(Ip_Dst, Ip, Mac);
	{answer, Ip_Dst, Mac_Dst} ->
	    send_arp(Ip_Dst, Mac_Dst, Ip, Mac)
    end,
    writer_loop(Ip, Mac).

%%%%%%%%%%%%% Reader Help Functions %%%%%%%%%%%%%%%%%%%%%

decode(Packet, Ip, Mac) ->
    case Packet of 
	<<1:16/big-integer, ?ETH_IP:16/big-integer,
	 ?ETH_LEN:8/integer, ?IP_LEN:8/integer,
	 ?ARP_REQUEST:16/big-integer, Src_Mac:48/big-integer,
	 Src_Ip:32/big-integer, _:48/big-integer,
	 Ip:32/big-integer,_/binary>> -> % Incoming Arp Request
	    ets:insert(arp_cache, {Src_Ip, Src_Mac}), 
	    answer(Src_Ip, Src_Mac);

	<<1:16/big-integer, ?ETH_IP:16/big-integer,
	 ?ETH_LEN:8/integer, ?IP_LEN:8/integer, 
	 ?ARP_REPLY:16/big-integer, Src_Mac:48/big-integer,
	 Src_Ip:32/big-integer, Mac:48/big-integer, 
	 Ip:32/big-integer,_/binary>> -> % Incoming Arp Reply
	    ets:insert(arp_cache, {Src_Ip, Src_Mac}),
	    recv_reply(Src_Ip, Src_Mac)
end.

%%%%%%%%%%%% Writer Help Functions %%%%%%%%%%%%%%%%%%%%%%

send_arp(Ip_Dst, Ip, Mac) -> % Send Arp Request
    eth:send(make_packet(request, Ip_Dst, ?ETH_BROAD, Ip, Mac),
	     arp, ?ETH_BROAD).
send_arp(Ip_Dst, Mac_Dst, Ip, Mac) -> % Send Arp Reply
    eth:send(make_packet(reply, Ip_Dst, Mac_Dst, Ip, Mac),
	     arp, Mac_Dst).

make_packet(Type, Ip_Dst, Mac_Dst, Ip, Mac) ->
    Num_Type = type(Type),
    <<1:16/big-integer, ?ETH_IP:16/big-integer,
     ?ETH_LEN:8/integer, ?IP_LEN:8/integer,
     Num_Type:16/big-integer, Mac:48/big-integer,
     Ip:32/big-integer, Mac_Dst:48/big-integer,
     Ip_Dst:32/big-integer>>.

type(request) -> ?ARP_REQUEST;
type(reply) ->   ?ARP_REPLY.

%%%%%%%%%%%%%% Ip Queue Help Functions %%%%%%%%%%%%%%%%%%

queue(Ip_Addr, Packet) ->
    ets:insert(ip_queue, {Ip_Addr, Packet}).

dequeue(Ip_Addr) ->
    case catch ets:lookup_element(ip_queue, Ip_Addr, 2) of
		Packets when is_list(Packets) ->
			ets:delete(ip_queue, Ip_Addr),
			Packets;
		_ ->
			[]
    end.

send_packet(Packet, Dst_Mac) ->
    eth:send(Packet, ip, Dst_Mac).
