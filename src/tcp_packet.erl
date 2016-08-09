%%%-------------------------------------------------------------------
%%% File    : tcp_packet.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp Packet Parser and Generator
%%%
%%% Created : 17 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(tcp_packet).

-include("tcp_packet.hrl").
-include("ip.hrl").

-import(packet_check, [check_packet/4, compute_checksum/5]).
-export([parse/3, send_packet/2, send_packet/1]).

-define(DEFAULT_HDLEN, 5).

%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(Src_Ip, Dst_Ip, Packet) ->
    case check_packet(Src_Ip, Dst_Ip, ?IP_PROTO_TCP, Packet) of
	ok ->
	    parse_packet(Src_Ip, Dst_Ip, Packet);
	{error, Error} ->
	    {error, Error}
    end.

send_packet(Tcb, rto) -> % Retransmit packet
    case tcb:get_tcbdata(Tcb, rqueue) of
	empty ->
	    ok;
	Packet ->
	    Smss = tcb:get_tcbdata(Tcb, smss),
	    retransmit(Tcb, Packet, Smss)
    end;
send_packet(Tcb, Type) ->
    case build_packet(Tcb, Type) of
	{error, Error} ->
	    {error, Error};
	{ok, Pkt, Data_Avail} ->
	    send_packet_1(Tcb, Pkt),
	    {ok, Data_Avail}
    end.

send_packet(Pkt) ->
    {Packet, Len} = build_bin_packet(Pkt),
    ip:send(Packet, Len, tcp, Pkt#pkt.dip).

%%%%%%%%%%%%%%%%%%%% PACKET PARSING %%%%%%%%%%%%%%%%%%%%

%% Takes data from binary packet to a pkt record

parse_packet(Src_Ip, Dst_Ip, Packet) ->
    <<SPort:16/big-integer,
      DPort:16/big-integer,
      Seq:32/big-integer,
      Ack:32/big-integer,
      Off:4/big-integer,
      _:6/big-integer,
      Is_Urg:1/integer,
      Is_Ack:1/integer,
      Is_Psh:1/integer,
      Is_Rst:1/integer,
      Is_Syn:1/integer,
      Is_Fin:1/integer,
      Window:16/big-integer,
      _Checksum:16/big-integer,
      Urgent:16/big-integer,
      Rem/binary>> = Packet,
    {Mss, Data} = get_options(Off, Rem), 
    {ok, #pkt{
      sip   = Src_Ip,
      dip   = Dst_Ip,
      sport = SPort,
      dport = DPort,
      seq   = Seq,
      ack   = Ack,
      is_urg= Is_Urg,
      is_ack= Is_Ack,
      is_psh= Is_Psh,
      is_rst= Is_Rst,
      is_syn= Is_Syn,
      is_fin= Is_Fin,
      window= Window,
      urgent= Urgent,
      mss   = Mss,
      data  = Data,
      data_size = size(Data)
     }}.

%% Separates options and Data

get_options(Off, Rem) ->
    case Off of
	5 -> % 20 bytes, ergo no options
	    Smss = -1,
	    Data = Rem;
	X when X > 5 ->
	    Opt_Size = (Off-5) * 4,
	    <<Options:Opt_Size/binary,Data/binary>> = Rem,
	    Smss = parse_options(Options);
	_ -> % Offset point to packet ??. Should return an error
	    Smss = -1,
	    Data = Rem
    end,
    {Smss, Data}.

%% Options Parser

parse_options(<<>>) -> % Should parse more options
    -1;
parse_options(Options) ->
    <<Kind:8/integer,
      Rem/binary>> = Options,
    case Kind of
	0 -> % End of options
	    -1;
	1 -> % Nop
	    parse_options(Rem);
	2 -> % Mss
	    <<_Len:8/integer,
	      Smss:16/integer,
	      _/binary>> = Rem,
	    Smss;
	_ ->
	    <<Len:8/integer, Rem_2/binary>> = Rem,
	    Size = Len - 2,
	    <<_:Size/binary, Rest/binary>> = Rem_2,
	    parse_options(Rest)
    end.


%%%%%%%%%%%%%%%%%%% PACKET BUILDING %%%%%%%%%%%%%%%%%%%%%%%%%

send_packet_1(Tcb, Pkt) ->
    {Bin_Packet, Len} = build_bin_packet(Tcb, Pkt),
    ip:send(Bin_Packet, Len, tcp, Pkt#pkt.dip),
    
    Seq_Len = size(Pkt#pkt.data)+Pkt#pkt.is_syn+Pkt#pkt.is_fin,
    prepare_retransmit(Tcb, Pkt#pkt.seq, Seq_Len, Pkt),
    if
	(Pkt#pkt.is_syn == 1) or (Pkt#pkt.is_fin == 1) ->
	    tcb:set_tcbdata(Tcb, snd_nxt, 1);
	true ->
	    ok
    end.

prepare_retransmit(Tcb, Snd_Nxt, Seq_Len, Packet) when Seq_Len > 0 ->
    tcb:set_tcbdata(Tcb, rqueue, {seq:add(Snd_Nxt, Seq_Len), Packet});
prepare_retransmit(_, _, _, _) ->
    ok.

build_bin_packet(Tcb, Pkt) ->
    case Pkt#pkt.is_syn of
	1 ->
	    Rmss = tcb:get_tcbdata(Tcb, rmss),
	    Options = <<2:8/integer,
		       4:8/integer,
		       Rmss:16/big-integer>>,
	    Hd_Len = 6; % Check this later when other options are added
	0 ->
	    Options = <<>>,
	    Hd_Len = ?DEFAULT_HDLEN
    end,
    {Pre_Chk, Post_Chk} = build_bin_packet_1(Pkt, Hd_Len),
    add_checksum(Pkt#pkt.sip, Pkt#pkt.dip, Pre_Chk, Post_Chk, 
		 Options, Pkt#pkt.data, Pkt#pkt.data_size).

build_bin_packet(Pkt) ->
    {Pre_Chk, Post_Chk} = build_bin_packet_1(Pkt, ?DEFAULT_HDLEN),
    add_checksum(Pkt#pkt.sip, Pkt#pkt.dip, Pre_Chk, Post_Chk,
		 <<>>, Pkt#pkt.data, Pkt#pkt.data_size).

build_bin_packet_1(Pkt, Hd_Len) ->
    {<<(Pkt#pkt.sport):16/big-integer,
     (Pkt#pkt.dport):16/big-integer,
     (Pkt#pkt.seq):32/big-integer,
     (Pkt#pkt.ack):32/big-integer,
     Hd_Len:4/integer,
     0:6/integer, % Reserved
     (Pkt#pkt.is_urg):1/integer,
     (Pkt#pkt.is_ack):1/integer,
     (Pkt#pkt.is_psh):1/integer,
     (Pkt#pkt.is_rst):1/integer,
     (Pkt#pkt.is_syn):1/integer,
     (Pkt#pkt.is_fin):1/integer,
     (Pkt#pkt.window):16/big-integer>>,
     <<(Pkt#pkt.urgent):16/big-integer>>}.

add_checksum(Src_Ip, Dst_Ip, Pre_Chk, Post_Chk, Options, Data, Data_Size) ->
    Checksum = compute_checksum(Src_Ip, Dst_Ip, ?IP_PROTO_TCP,
				[Pre_Chk, Post_Chk, Options, Data],
				20+size(Options) + Data_Size),
    {[Pre_Chk, <<Checksum:16/big-integer>>, Post_Chk, Options, Data], 
     20+size(Options) + Data_Size}.

% Ack, Psh, Rst, Syn, Fin, Snd_Nxt, Data

build_packet(Tcb, syn) ->
    build_packet_1(Tcb, nodata, 0, 0, 0, 1, 0);
build_packet(Tcb, synack) ->
    build_packet_1(Tcb, nodata, 1, 0, 0, 1, 0);
build_packet(Tcb, data) ->
    {ok, Pkt, Data_Avail} = build_packet_1(Tcb, data, 1, 1, 0, 0, 0),
    if 
	size(Pkt#pkt.data) == 0 ->
	    {error, nodata};
	true ->
	    {ok, Pkt, Data_Avail}
    end;
build_packet(Tcb, ack) ->
    {ok, Pkt, Data_Avail} = build_packet_1(Tcb, data, 1, 0, 0, 0, 0),
    if
	size(Pkt#pkt.data) > 0 ->
		  {ok, Pkt#pkt{is_psh=1}, Data_Avail};
	true ->
		  {ok, Pkt, Data_Avail}
    end;
build_packet(Tcb, fin) ->
    build_packet_1(Tcb, nodata, 1, 0, 0, 0, 1);
build_packet(Tcb, rst) ->
    build_packet_1(Tcb, nodata, 1, 0, 1, 0, 0).

build_packet_1(Tcb, Type, Is_Ack, Is_Psh, Is_Rst, Is_Syn, Is_Fin) ->
    case Type of 
	nodata ->
	    {_, Snd_Nxt, _, _} = tcb:get_tcbdata(Tcb, snd),
	    {Lc_Ip, Lc_Port, Rt_Ip, Rt_Port,
	     Rcv_Nxt, Rcv_Wnd} = tcb:get_tcbdata(Tcb, socket),
	    Data = <<>>,
	    Data_Size = 0,
	    Data_Avail = 0;
	data ->
	    {Snd_Nxt, Data_Avail, Data, Data_Size, Lc_Ip, Lc_Port, Rt_Ip, 
	     Rt_Port, Rcv_Nxt, Rcv_Wnd} = tcb:get_tcbdata(Tcb, sdata)
    end,
    Pkt = #pkt{sip   = Lc_Ip,
	       dip   = Rt_Ip,
               sport = Lc_Port,
	       dport = Rt_Port,
	       seq   = Snd_Nxt,
	       ack   = Rcv_Nxt,
	       is_urg= 0,
	       is_ack= Is_Ack,
	       is_psh= Is_Psh,
	       is_rst= Is_Rst,
	       is_syn= Is_Syn,
	       is_fin= Is_Fin,
	       window= Rcv_Wnd,
	       urgent= 0,
	       data = Data,
	       data_size = Data_Size},
    {ok, Pkt, Data_Avail}.

%% Retransmissions

retransmit(Tcb, Packet, Smss) when Packet#pkt.data_size =< Smss ->
    {Bin_Packet, Len} = build_bin_packet(Tcb, Packet),
    ip:send(Bin_Packet, Len, tcp, Packet#pkt.dip);
retransmit(Tcb,Packet, Smss) ->
    <<Data:Smss/binary, Rem/binary>> = Packet#pkt.data,
    Send_Packet= Packet#pkt{data = Data, data_size = Smss},
    
    {Bin_Packet, Len} = build_bin_packet(Tcb, Send_Packet),
    ip:send(Bin_Packet, Len, tcp, Send_Packet#pkt.dip),
    retransmit(Tcb, Packet#pkt{seq = seq:add(Packet#pkt.seq,Smss), 
			       data = Rem, data_size = size(Rem)}, Smss).   
