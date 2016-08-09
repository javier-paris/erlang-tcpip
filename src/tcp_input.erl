%%%-------------------------------------------------------------------
%%% File    : tcp_input.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp input packet processing
%%%
%%% Created :  6 Sep 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(tcp_input).

-export([check_ack/2, process_packet/4]).

-include("tcp_packet.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_ack(Tcb, Pkt) ->
    {Snd_Una, Snd_Nxt, _, _} = tcb:get_tcbdata(Tcb, snd),
    case Pkt#pkt.is_ack of
	1 ->
	    Dup_Ack = seq:le(Pkt#pkt.ack, Snd_Una),
	    New_Data = (not Dup_Ack) andalso
		       seq:le(Pkt#pkt.ack, Snd_Nxt),
	    if
		New_Data ->
		    tcb:set_tcbdata(Tcb, snd_una, Pkt#pkt.ack),
		    {ok, newdata};
		true ->
		    if
			Dup_Ack ->
			    {ok, nonewdata};
			true ->
			    {error, badack}
		    end
	    end;
	0 ->
	    {ok, noack}
    end.

process_packet(Tcb, Pkt, State, Writer) ->
    case get_data(Tcb, Pkt) of
	{ok, Rcv_Nxt, Data} -> % Data acceptable
	    process_rst(Tcb, Pkt, Writer, State, Rcv_Nxt, Data);
	{error, _} -> % Sequence number not correct, just discard
	    if
		Pkt#pkt.is_rst -> % Just drop
		    ok;
		true ->
		    tcp_con:send_packet(Writer, ack)
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%

process_rst(Tcb, Pkt, Writer, State, Rcv_Nxt, Data) ->
    case Pkt#pkt.is_rst of
	1 ->
	    tcp_con:abort_connection(); % Make this function
	0 ->
	    process_syn(Tcb, Pkt, Writer, State, Rcv_Nxt, Data)
    end.

process_syn(Tcb, Pkt, Writer, State, Rcv_Nxt, Data) ->
    case Pkt#pkt.is_syn of
	1 ->
	    tcp_con:abort_connection();
	0 -> 
	    process_ack(Tcb, Pkt, Writer, State, Rcv_Nxt, Data)
    end.

process_ack(Tcb, Pkt, Writer, State, Rcv_Nxt, Data) ->
    case check_ack(Tcb, Pkt) of
	{ok, newdata} ->
	    process_window(Tcb, Pkt),
	    process_data(Tcb, Pkt, Writer, State, Rcv_Nxt, Data),
	    State:newdata_action(Tcb, Pkt, Writer);
	{ok, nonewdata} ->
	    case State:nonewdata_action(Tcb, Pkt, Writer) of
		ok ->
		    process_window(Tcb, Pkt),
		    process_data(Tcb, Pkt, Writer, State, Rcv_Nxt, Data);
		_ ->
		    ok
	    end;
	{ok, noack} -> % Packet should have an ack, so drop
	    ok;
	{error, badack} ->
	    State:badack_action(Tcb, Pkt, Writer)
    end.

process_window(Tcb, Pkt) ->
    tcb:set_tcbdata(Tcb, snd_wnd,
		    {Pkt#pkt.window, Pkt#pkt.seq, Pkt#pkt.ack}).

process_data(Tcb, Pkt, Writer, State, Rcv_Nxt, <<>>) ->
    process_fin(Tcb, Pkt#pkt.is_fin, Writer, Rcv_Nxt, State, no_ack, 0);
process_data(Tcb, Pkt, Writer, State, Rcv_Nxt, Data) ->
    if
	Rcv_Nxt < Pkt#pkt.seq ->  % Out of order data
	    Out_Order_Data = {Pkt#pkt.seq, Pkt#pkt.is_fin, Data},
	    State:out_order_action(Tcb, Out_Order_Data, Writer);
	true ->
	    case State:data_action(Tcb, Data) of
		ok ->
		    NRcv_Nxt = seq:add(Rcv_Nxt, size(Data)),
		    check_out_order(Tcb, NRcv_Nxt, State, size(Data), 
				    Pkt, Writer);
		_ ->
		    ok
	    end
    end.

check_out_order(Tcb, Rcv_Nxt, State, Data_Size, Pkt, Writer) ->
    case tcb:get_tcbdata(Tcb, out_order) of
	{_, Is_Fin, Data} ->
	    State = tcb:get_tcbdata(Tcb, state),
	    State:data_action(Tcb, Data),
	    process_fin(Tcb, Is_Fin, Writer, Rcv_Nxt, State,
			ack, size(Data));
	_ ->
	    process_fin(Tcb, Pkt#pkt.is_fin, Writer, Rcv_Nxt, State,
			del_ack, Data_Size)
    end.

process_fin(Tcb, Is_Fin, Writer, Rcv_Nxt, State, Ack, Data_Size) ->
  case Is_Fin of
      1 ->
	  State:fin_action(Tcb, Rcv_Nxt, Writer);
      0 ->
	  case Ack of
	      ack ->
		  tcp_con:send_packet(Writer, ack);
	      del_ack ->
		  tcb:set_tcbdata(Tcb, del_ack, Data_Size);
	      no_ack ->
		  ok
	  end
  end.

get_data(Tcb, Pkt) ->
    Seg_Len = size(Pkt#pkt.data),
    {Rcv_Nxt, Rcv_Wnd, _} = tcb:get_tcbdata(Tcb, rcv),
    if   % 99% of packets should fall in the first condition
	(Pkt#pkt.seq == Rcv_Nxt) and (Seg_Len =< Rcv_Wnd) ->
	    {ok, Rcv_Nxt, Pkt#pkt.data};
	true ->
	    case {Seg_Len, Rcv_Wnd} of
		{_, 0} -> % Check if the sequence number is the one
		    case Pkt#pkt.seq == Rcv_Nxt of
			true ->
			    {ok, Rcv_Nxt, <<>>};
			false ->
			    {error, badseq}
		    end;
		{0, _} -> % Check if sequence number is in window
		    case seq:le(Rcv_Nxt, Pkt#pkt.seq) andalso
			seq:lt(Pkt#pkt.seq, seq:add(Rcv_Nxt, Rcv_Wnd)) of
			true ->
			    {ok, Rcv_Nxt, <<>>};
			false ->
			    {error, badseq}
		    end;
		{_, _} -> % Check that it either starts or ends in the window
		    trim_packet(Tcb, Pkt, Rcv_Nxt, Rcv_Wnd, Seg_Len)
	    end
    end.

%% Cuts data that is not in the window

trim_packet(_, Pkt, Rcv_Nxt, Rcv_Wnd, Seg_Len) ->
    Pkt_End = seq:add(Pkt#pkt.seq, Seg_Len),
    Wnd_End = seq:add(Rcv_Nxt, Rcv_Wnd),
    
    Start = seq:max(Pkt#pkt.seq, Rcv_Nxt),
    End = seq:min(Wnd_End, Pkt_End),
    
    case seq:gt(Start, End) of
	true ->
	    {error, badseq};
	false ->
	    Size = End - Start,
	    Off = case seq:gt(Rcv_Nxt, Pkt#pkt.seq) of
		      true ->
			  seq:sub(Rcv_Nxt, Pkt#pkt.seq);
		      false ->
			  0
		  end,
	    <<_:Off/binary,Data:Size/binary,_/binary>> = Pkt#pkt.data,
	    {ok, Rcv_Nxt, Data}
    end.
