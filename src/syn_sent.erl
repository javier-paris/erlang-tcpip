%%%-------------------------------------------------------------------
%%% File    : syn_sent.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp syn sent connection state
%%%
%%% Created : 18 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(syn_sent).

-export([recv/3, send/2, queue/0, read/2, close/0]).

-include("tcp_packet.hrl").

%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    case tcp_input:check_ack(Tcb, Pkt) of
	{ok, newdata} ->
	    recv_1(Tcb, Pkt, Writer, newdata);
	{ok, nonewdata} ->
	    tcp_con:send_packet(Writer, rst);
	{ok, noack} ->
	    recv_1(Tcb, Pkt, Writer, noack);
	{error, badack} ->
	    tcp_con:send_packet(Writer, rst)
    end.

recv_1(Tcb, Pkt, Writer, Ack_State) ->
    case Pkt#pkt.is_rst of
	1 ->
	    case Ack_State of
		newdata ->
		    tcp_con:abort(Tcb, connection_reset);
		_ ->
		    ok
	    end;
	0 ->
	    recv_2(Tcb, Pkt, Writer, Ack_State)
    end.

recv_2(Tcb, Pkt, Writer, Ack_State) ->
    case Pkt#pkt.is_syn of
	1 ->
	    tcb:set_tcbdata(Tcb, rcv_nxt, seq:add(Pkt#pkt.seq, 1)),
	    tcb:set_tcbdata(Tcb, irs, Pkt#pkt.seq),
	    case Ack_State of
		newdata ->
		    case Pkt#pkt.mss of
			-1 ->
			    ok;
			Smss ->
			    tcb:set_tcbdata(Tcb, smss, Smss),
			    tcb:set_tcbdata(Tcb, cwnd, 2*Smss)
		    end,
		    tcb:set_tcbdata(Tcb,snd_wnd,{Pkt#pkt.window,
						 Pkt#pkt.seq,
						 Pkt#pkt.ack}),
		    tcp_con:send_packet(Writer, ack),
		    tcb:syncset_tcbdata(Tcb, state, established);
		noack ->
		    tcb:syncset_tcbdata(Tcb, state, syn_received),
		    tcp_con:send_packet(Writer, synack)
	    end;
	0 ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Tcb, {send, ack}) ->
    tcp_packet:send_packet(Tcb, ack);
send(Tcb, {send, synack}) ->
    tcp_packet:send_packet(Tcb, synack);
send(Tcb, rto) -> % Retransmit timeout
    tcp_packet:send_packet(Tcb, rto);
send(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%%

queue() ->
    ok.

read(_, Bytes) ->
    {ok, Bytes}.

close() ->
    ok.
