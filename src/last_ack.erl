%%%-------------------------------------------------------------------
%%% File    : last_ack.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : 
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

-module(last_ack).

-export([recv/3, send/2, badack_action/3, newdata_action/3,
	 nonewdata_action/3, data_action/2, fin_action/3,
	 out_order_action/3, queue/0, read/2, close/0]).

%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    tcp_input:process_packet(Tcb, Pkt, last_ack, Writer).

%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Tcb, rto) -> % Only send retransmitions in this state
    tcp_packet:send_packet(Tcb, rto);
send(_, _) -> 
    ok.

%%%%%%%%%%%%%%%%%% TCP INPUT CALLBACKS %%%%%%%%%%%%%%%%%%%%

badack_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

nonewdata_action(_, _, _) ->
    ok.

newdata_action(Tcb, _, _) ->
    {Snd_Una, Snd_Nxt, _, _} = tcb:get_tcbdata(Tcb, snd),
    if
	Snd_Una == Snd_Nxt -> % All data acked, close
	    tcp_con:close_connection(Tcb),
	    ok;
	true ->
	    no_data_action
    end.

data_action(_, _) ->
    ok.

out_order_action(_, _, _) ->
    ok.

fin_action(_, _, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%

queue() ->
    {error, connection_closing}.

read(_, _) ->
    {error, connection_closing}.

close() ->
    {error, connection_closing}.
