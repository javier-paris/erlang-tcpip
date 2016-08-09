%%%-------------------------------------------------------------------
%%% File    : packet_check.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Packet checker for udp and tcp.
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

-module(packet_check).

-import(checksum, [checksum/1, checksum_1/1]).
-export([check_packet/4, compute_checksum/5]).

-define(BIG_PACKET, 100).

check_packet(Src_Ip, Dst_Ip, Protocol, Packet) ->
    Size = size(Packet),
    Chk_Packet = build_checksum_packet(Src_Ip, Dst_Ip, Protocol, 
				       Packet, Size),
    R = if Size < ?BIG_PACKET ->
		checksum_1(Chk_Packet);
	   true ->
		checksum(Chk_Packet)
	end,
    case R of
	0 ->
	    ok;
	_X ->
	    {error, bad_checksum}
    end.

build_checksum_packet(Src_Ip, Dst_Ip, Protocol, Packet, Len) ->
    Pad = case Len rem 2 of   % It must have even length
	      0 -> % Even Length
		  <<>>;
	      1 -> % Odd Length
		  <<0:8/integer>>
	  end,
    [<<Src_Ip:32/big-integer,
       Dst_Ip:32/big-integer,
       0:8/integer,
       Protocol:8/integer,
       Len:16/big-integer>>,
     Packet,
     Pad].

compute_checksum(Src_Ip, Dst_Ip, Protocol, Packet, Size) ->
    Chk_Packet = build_checksum_packet(Src_Ip, Dst_Ip, Protocol, Packet, Size),
    checksum(Chk_Packet).
