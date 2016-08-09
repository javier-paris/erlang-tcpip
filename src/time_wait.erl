%%%-------------------------------------------------------------------
%%% File    : time_wait.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp time-wait connection state
%%%
%%% Created :  9 Sep 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(time_wait).

-export([recv/3, send/2, badack_action/3, newdata_action/3,
	 nonewdata_action/3, data_action/2, fin_action/3,
	 out_order_action/3, queue/0, read/2, close/0]).

-include("tcp_packet.hrl").

%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    tcb:set_tcbdata(Tcb, twtimer, Writer),
    tcp_input:process_packet(Tcb, Pkt, time_wait, Writer).

%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Tcb, {send, ack}) ->
    tcp_packet:send_packet(Tcb, ack);
send(Tcb, time_wait) ->
    tcp_con:close_connection(Tcb);
send(_, _) -> 
    ok.

%%%%%%%%%%%%%%%%% TCP INPUT CALLBACKS %%%%%%%%%%%%%%%%%%%%%%

badack_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

nonewdata_action(_, _, _) ->
    ok.

newdata_action(_, _, _) ->
    ok.

data_action(_, _) ->
    no_data_action.

out_order_action(_, _, _) ->
    ok.

fin_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%

queue() ->
    {error, connection_closing}.

read(_, _) ->
    {error, connection_closing}.

close() ->
    {error, connection_closing}.
