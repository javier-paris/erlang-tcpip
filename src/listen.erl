%%%-------------------------------------------------------------------
%%% File    : listen.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp Listen connection state
%%%
%%% Created :  3 Sep 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(listen).

-export([recv/3, send/2, queue/0, read/2, close/0]).

-include("tcp_packet.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    case Pkt#pkt.is_rst of % Discard rsts
	1 ->
	    ok;
	0 ->
	    process_ack(Tcb, Pkt, Writer)
    end.

process_ack(Tcb, Pkt, Writer) ->
    case Pkt#pkt.is_ack of % Send an rst to an ack packet
	1 ->
	    rst(Tcb, Pkt); 
	0 ->
	    process_syn(Tcb, Pkt, Writer)
    end.

process_syn(Tcb, Pkt, _Writer) ->
    case Pkt#pkt.is_syn of
	1 ->
	    {N_Tcb, N_Reader, N_Writer} = tcp_con:clone(Tcb),
	    tcb:syncset_tcbdata(Tcb, syn_queue, {N_Tcb, N_Reader, N_Writer}),
	    
	    tcb:set_tcbdata(N_Tcb, rsocket, {Pkt#pkt.sip, Pkt#pkt.sport}),
	    tcb:set_tcbdata(N_Tcb, rcv_nxt, seq:add(Pkt#pkt.seq, 1)),
	    tcb:set_tcbdata(N_Tcb, irs, Pkt#pkt.seq),
	    tcb:syncset_tcbdata(N_Tcb, state, syn_rcvd),
	    Socket = {Pkt#pkt.dip, Pkt#pkt.dport, 
		      Pkt#pkt.sip, Pkt#pkt.sport},
	
	    tcp_pool:add({connect, Socket}, {N_Tcb, N_Reader, N_Writer}),
	    tcp_con:send_packet(N_Writer, synack);
	0 ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(_, _) ->
    ok.

rst(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%

queue() ->
    {error, no_connection}.

read(_, _) ->
    {error, no_connection}.

close() ->
    ok.
