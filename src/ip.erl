%%%-------------------------------------------------------------------
%%% File    : ip.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Ip Protocol Layer
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

-module(ip).

-import(checksum,[checksum/1, checksum_1/1]).
-export([start/3,init_reader/2,init_writer/3,recv/1,send/4, fragment/4,
	 change_mtu/2, dst_unreachable/1, get_mtu/0]).

-include("ip.hrl").

-define(TTL, 128).
-define(INT16, 65535).

%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Ip_Addr, NetMask, Default_Gateway) ->
    init(Ip_Addr, NetMask, Default_Gateway).

recv(Packet) ->
    catch ip_reader ! {recv, Packet}.

fragment(Frg_Id, Src_Ip, Protocol, Data) ->  % Completed fragmented packet
    catch ip_reader ! {fragment, Frg_Id, Src_Ip, Protocol, Data}.

send(Packet, Len, Protocol, Ip_Addr) ->
    catch ip_writer ! {send, Packet, Len, Protocol, Ip_Addr}.

get_mtu() ->
    eth:get_mtu(). % Should probably implement some way of having different link layer protocols here

change_mtu(Packet, MTU) ->
    catch ip_reader ! {mtu, Packet, MTU}.

%% Destination Unreachable received. Parse the packet and 
%% notify the upper level protocol.
dst_unreachable(Packet) ->
    catch ip_reader ! {dst_unr, Packet}.

%%%%%%%%%% Reader and Writer Loops %%%%%%%%%%%%

init(Ip_Addr, NetMask, Default_Gateway) ->
    spawn_link(ip, init_writer, [Ip_Addr, NetMask, Default_Gateway]),
    spawn_link(ip, init_reader, [Ip_Addr, NetMask]).

init_reader(Ip_Addr, NetMask) ->
    register(ip_reader, self()),
    ets:new(ip_fragment, [set, private, named_table]),
    ets:new(mtu, [set, public, named_table]),
    reader_loop(Ip_Addr, NetMask).

init_writer(Ip_Addr, NetMask, Default_Gateway) ->
    register(ip_writer, self()),
    put(frg_id, 1),
    writer_loop(Ip_Addr, NetMask, Default_Gateway).

reader_loop(Ip_Addr, NetMask) ->
    receive
		{recv, Packet} when is_binary(Packet) ->
			process_incoming_packet(recv, Packet, Ip_Addr, NetMask);
		{fragment, Frg_Id, Src_Ip, Protocol, Data} ->
			delete_fragment(Frg_Id, Src_Ip),
			pop(recv, Protocol, Src_Ip, Ip_Addr, Data);
		{mtu, Packet, MTU} -> % An ICMP DF Set and Must fragment Received
			update_mtu(Packet, MTU, Ip_Addr);
		{dst_unr, Packet} ->
			process_incoming_packet(dst_unr, Packet, Ip_Addr, NetMask);
		_ ->
			{error, failed}
    end,
    reader_loop(Ip_Addr, NetMask).

writer_loop(Ip_Addr, NetMask, Default_Gateway) ->
	receive
		{send, Packet, Len, Protocol, Dst_Ip} ->
			send_packet(Packet, Len, Protocol, Dst_Ip, Ip_Addr, NetMask, Default_Gateway)
    end,
    writer_loop(Ip_Addr, NetMask, Default_Gateway).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_packet(Data, Len, Protocol, Dst_Ip, Ip_Addr, NetMask, Default_Gateway) ->
    {Gateway, DF} = route(Dst_Ip, Ip_Addr, NetMask, Default_Gateway),
    {Pre_Chk, Post_Chk} = build_header(Len, Protocol, Dst_Ip, Ip_Addr, DF),
    Checksum = checksum_1([Pre_Chk, Post_Chk]),
    Packet = [Pre_Chk, <<Checksum:16/integer>>, Post_Chk, Data],
    arp:send(Packet, Gateway).

%% Header is built as a tuple with two binaries which are the parts that come before and after the checksum.
%% Checksum is computed using a c driver for speed
build_header(Data_Size, Protocol, Dst_Ip, Ip_Addr, DF) -> 
    Len = Data_Size + 20,
    Frg_Id = get_frg_id(),
    Num_Protocol = protocol(Protocol),
    {<<4:4/integer,            % Ip Version
      5:4/integer,             % Header Length -- No options for now
      0:8/integer,             % Tos
      Len:16/big-integer,      % Packet Length
      Frg_Id:16/big-integer,   % Fragment Id
      0:1/integer,             % Reserved
      DF:1/integer,            % Don't Fragment. Right now it is set to 1 and PMTU is used, but there should be an option to set it to 0 for UDP packets
      0:1/integer,             % More Fragments
      0:13/big-integer,        % Fragment Offset
      ?TTL:8/integer,          % TTL
      Num_Protocol:8/integer>>,% Protocol Number
      <<Ip_Addr:32/big-integer,% Source Ip
      Dst_Ip:32/big-integer>>}.% Destination Ip

protocol(icmp) -> ?IP_PROTO_ICMP;
protocol(tcp) ->  ?IP_PROTO_TCP;
protocol(udp) ->  ?IP_PROTO_UDP;
protocol(?IP_PROTO_ICMP) -> icmp;
protocol(?IP_PROTO_TCP) -> tcp;
protocol(?IP_PROTO_UDP) -> udp.

route(Dst_Ip, Ip_Addr, NetMask, Default_Gateway) -> % To be rewritten
    case ets:lookup(mtu, Dst_Ip) of
	[{_, GateWay, _, DF}] ->
	    {GateWay, DF};
	_ ->
	    {mtu, MTU} = eth:get_mtu(),
	    if 
		(Dst_Ip band NetMask) == (Ip_Addr band NetMask) ->
		    ets:insert(mtu, {Dst_Ip, Dst_Ip, MTU, 1}),
		    {Dst_Ip, 1};
		true ->
		    ets:insert(mtu, {Dst_Ip, Default_Gateway, MTU, 1}),
		    {Default_Gateway, 1}
	    end
    end.

get_frg_id() ->
    Frg_Id = get(frg_id),
    put(frg_id, (Frg_Id + 1) rem ?INT16),
    Frg_Id.
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_incoming_packet(Type, Packet, Ip_Addr, NetMask) ->
    case catch decode(Packet) of
	{ok, Frg_Id, Mf, Offset, Protocol, Src_Ip, Dst_Ip, Data} ->
	    case check_address(Dst_Ip, Ip_Addr, NetMask) of
		ok ->
		    case defragment(Frg_Id, Mf, Offset, Protocol,
				    Src_Ip, Data) of
			single_packet ->
			    pop(Type, Protocol, Src_Ip, Ip_Addr, Data);
			fragment ->
			    ok
		    end;
		{error, Error} ->
		    {error, Error}
	    end;
	{error, Error} ->
	    {error, Error};
	_ ->
	    {error, failed}
    end.

update_mtu(Packet, MTU, Ip_Addr) ->
    case catch decode(Packet) of
	{ok, _, _, _, Protocol, Src_Ip, Dst_Ip, Data} ->
	    if 
			Src_Ip == Ip_Addr -> % We sent this
				case ets:lookup(mtu, Src_Ip) of
				[{_, GateWay, Old_MTU, _DF}] ->
					if
					MTU == 0 ->
						ets:insert(mtu, {Src_Ip, GateWay, Old_MTU, 0}),
						pop(new_mtu, Protocol, Ip_Addr, 
						Dst_Ip, {Data, MTU});
					MTU < Old_MTU ->
						ets:insert(mtu, {Src_Ip, GateWay, MTU, 1}),
						pop(new_mtu, Protocol, Ip_Addr, 
						Dst_Ip, {Data, MTU});
					true ->
						ok
					end;
				_ -> % Just Ignore
					ok
				end;
			true ->
				{error, notforus}
	    end;
	{error, Error} ->
	    io:format("Error:~w~n",[Error]),
	    {error, Error}
    end.

%% Asume no options for decoding at first.
decode(Packet) ->
    <<Header:20/binary,Data/binary>> = Packet,
    case checksum_1(Header) of
	16#0 -> % Checksum ok
	    analize(Header, Data);
	_ -> % Ok, look if we failed checksum due to options
	    <<_:4/integer, Hd_DW_Len:4/integer, _/binary>> = Packet,
	    Hd_Len = Hd_DW_Len * 4,
	    if
		Hd_Len == 5 ->
		    {error, bad_checksum};
		true ->
		    <<Header_1:Hd_Len/binary, Data_1/binary>> = Packet,
		    case checksum_1(Header_1) of
			16#0 ->
			    analize(Header_1, Data_1);
			_ ->
			    {error, bad_checksum}
		    end
	    end
    end.
   
analize(Header, Padded_Data) ->
    <<4:4/integer,          % Ip Version
     Hd_Len:4/integer,      % Header Length
     _:8/integer,           % Tos -- Ignored
     Len:16/big-integer,    % Packet Length
     Frg_Id:16/big-integer, % Fragment Id
     _:2/integer,           % Two first flags... ignored
     Mf:1/integer,          % More Fragments flag
     Offset:13/big-integer, % Fragment Offset
     _:8/integer,           % TTL -- We are not routing(yet) so we don't care
     Protocol:8/integer,    % Upper layer protocol
     _:16/big-integer,      % Checksum... Alredy checked
     Src_Ip:32/big-integer, % Source Ip
     Dst_Ip:32/big-integer, % Should check this against our Ip
     _Remainder/binary>> = Header, % We don't do options yet 
    Data_Len = Len - Hd_Len*4,
    if
	Data_Len < size(Padded_Data) ->
	    <<Data:Data_Len/binary, _/binary>> = Padded_Data;
	true ->
	    Data = Padded_Data
    end,
    {ok, Frg_Id, Mf, Offset, protocol(Protocol), Src_Ip, Dst_Ip, Data}.

check_address(Dst_Ip, Ip_Addr, NetMask) ->
    if 
	Dst_Ip == Ip_Addr ->
	    ok;
	(Dst_Ip bor NetMask) == 16#ffffffff -> % Broadcast
	    ok;
	(Dst_Ip == 0) -> % Broadcast -- Is this correct ??
	    ok;
	true ->
	    {error, notforus}
    end.

defragment(_Frg_Id, 0, 0, _Protocol, _Src_Ip, _Data) -> % Single Packet
    single_packet;
defragment(Frg_Id, MF, Offset, Protocol, Src_Ip, Data) ->
    case catch ets:lookup_element(ip_fragment, {Src_Ip, Frg_Id}, 2) of
	{'EXIT', _} ->
	    Fragment = fragment:start(Frg_Id, Protocol, Src_Ip),
	    ets:insert(ip_fragment, {{Src_Ip, Frg_Id}, Fragment}),
	    fragment:add(Fragment, Offset, Data, MF);
	Fragment ->
	    fragment:add(Fragment, Offset, Data, MF)
    end,
    fragment.

delete_fragment(Frg_Id, Src_Ip) ->
    catch ets:delete(ip_fragment, {Src_Ip, Frg_Id}).

pop(Type, tcp, Src_Ip, Ip_Addr, Data) ->
    tcp:Type(Src_Ip, Ip_Addr, Data);
pop(Type, udp, Src_Ip, Ip_Addr, Data) ->
    udp:Type(Src_Ip, Ip_Addr, Data);
pop(Type, icmp, Src_Ip, _Ip_Addr, Data) ->
    icmp:Type(Src_Ip, Data).
