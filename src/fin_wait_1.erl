%%%-------------------------------------------------------------------
%%% File    : fin_wait_1.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp fin-wait-1 connection state
%%%
%%% Created :  7 Sep 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(fin_wait_1).

-export([recv/3, send/2, badack_action/3, newdata_action/3,
	 nonewdata_action/3, data_action/2, fin_action/3,
	 out_order_action/3, queue/0, read/2, close/0]).

-include("tcp_packet.hrl").

%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    tcp_input:process_packet(Tcb, Pkt, fin_wait_1, Writer).

%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Tcb, {send, ack}) ->
    tcp_packet:send_packet(Tcb, ack);
send(Tcb, rto) ->
    tcp_packet:send_packet(Tcb, rto);
send(_, _) -> 
    ok.

%%%%%%%%%%%%%%%%% TCP INPUT CALLBACKS %%%%%%%%%%%%%%%%%%%%%%

badack_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

nonewdata_action(_, _, _) ->
    ok.

newdata_action(Tcb, _, _) ->
    {Snd_Una, Snd_Nxt, _, _} = tcb:get_tcbdata(Tcb, snd),
    State = tcb:get_tcbdata(Tcb, state),
    if
	Snd_Una == Snd_Nxt ->
	    if State == fin_wait_1 ->
		    tcb:syncset_tcbdata(Tcb, state, fin_wait_2),
		    ok;
	       true ->
		    ok
	    end;
	true ->
	    no_data_action
    end.
    
data_action(Tcb, Data) ->
    tcb:set_tcbdata(Tcb, rdata, Data).

out_order_action(Tcb, Data, Writer) ->
    tcb:set_tcbdata(Tcb, out_order, Data),
    tcp_con:send_packet(Writer, ack).

fin_action(Tcb, Rcv_Nxt, Writer) ->
    {Snd_Una, Snd_Nxt, _, _} = tcb:get_tcbdata(Tcb, snd),
    if
	Snd_Una == Snd_Nxt ->
	    tcb:syncset_tcbdata(Tcb, state, time_wait),
	    tcb:set_tcbdata(Tcb, twtimer, Writer);
	true ->
	    tcb:syncset_tcbdata(Tcb, state, closing)
    end,
    tcb:set_tcbdata(Tcb, rcv_nxt, seq:add(Rcv_Nxt, 1)),
    tcp_con:send_packet(Writer, ack).

%%%%%%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%

queue() ->
    {error, connection_closing}.

read(_, Bytes) ->
    {ok, Bytes}.

close() ->
    {error, connection_closing}.
