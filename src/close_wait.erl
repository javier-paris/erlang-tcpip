%%%-------------------------------------------------------------------
%%% File    : close_wait.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp close wait connection state
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

-module(close_wait).

-export([recv/3, send/2, badack_action/3, nonewdata_action/3, 
	 newdata_action/3, data_action/2, fin_action/3,
	 out_order_action/3, queue/0, read/2, close/0]).

%%%%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    tcp_input:process_packet(Tcb, Pkt, close_wait, Writer).

%%%%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Tcb, {send, ack}) ->
    tcp_packet:send_packet(Tcb, ack);
send(Tcb, {send, data}) ->
    tcp_packet:send_packet(Tcb, data);
send(Tcb, {send, fin}) ->
    Data_Size = tcb:get_tcbdata(Tcb, sbufsize),
    if
	Data_Size == 0 ->
	    tcb:syncset_tcbdata(Tcb, state, last_ack),
	    tcp_packet:send_packet(Tcb, fin);
	true ->
	    tcp_packet:send_packet(Tcb, data),
	    tcb:set_tcbdata(Tcb, send_fin, 1)
    end;
send(Tcb, rto) ->
    tcp_packet:send_packet(Tcb, rto);
send(_, _) -> 
    ok.

%%%%%%%%%%%%%%%%%%%%% TCP INPUT CALLBACKS %%%%%%%%%%%%%%%%%%%%%

badack_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

nonewdata_action(_, _, _) ->
    ok.

newdata_action(_, _, _) ->
    no_data_action.

data_action(_, _) ->
    ok.

out_order_action(_, _, _) ->
    ok.

fin_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

%%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

queue() ->
    ok.

read(Tcb, Bytes) ->
    case tcb:get_tcbdata(Tcb, rbufsize) of
	0 ->
	    {error, connection_closing};
	Size ->
	    {ok, min(Size, Bytes)}
    end.

close() ->
    ok.
