%%%-------------------------------------------------------------------
%%% File    : out_order.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Out of order Packet management
%%%
%%% Created : 18 Nov 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(out_order).

-export([new/0, merge_data/2, get_out_order/2]).

new() ->
    [].

merge_data([], Elem) ->
    [Elem];
merge_data(List = [{Lseq, Lis_Fin, Ldata}|T], Elem={Seq, Is_Fin, Data}) ->
    Pkt_Nxt = seq:add(Seq, size(Data)),
    LPkt_Nxt = seq:add(Lseq, size(Ldata)),
    
    if 
	Pkt_Nxt == Lseq ->
	    [{Seq, Lis_Fin, <<Data/binary, Ldata/binary>>} | T];
	LPkt_Nxt == Seq ->
	    New_Data = <<Ldata/binary, Data/binary>>,
	    merge_data(T, {Lseq, Is_Fin, New_Data});
	true ->
	    case seq:lt(Pkt_Nxt, Lseq) of
		true ->
		    [Elem | List];
		false ->
		    [{Lseq, Lis_Fin, Ldata} | merge_data(T, Elem)]
	    end
    end.

get_out_order([], _) ->
    {none, []};
get_out_order([{Lseq, Is_Fin, Data} | T], Seq) ->
    case seq:le(Lseq, Seq) of
	true ->
	    {{Lseq, Is_Fin, Data}, T};
	false ->
	    {none, T}
    end.
