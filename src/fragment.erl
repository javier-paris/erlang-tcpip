%%%-------------------------------------------------------------------
%%% File    : fragment.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Ip Fragment Holder
%%%
%%% Created :  9 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(fragment).

-export([start/3, init/3, add/4]).

start(Frg_Id, Protocol, Src_Ip) ->
    spawn(fragment, init, [Frg_Id, Protocol, Src_Ip]).

init(Frg_Id, Protocol, Src_Ip) ->
    loop(Frg_Id, Protocol, Src_Ip, [], false).

add(Fragment, Offset, Data, More) ->
    Fragment ! {add, Offset, Data, More}.

loop(Frg_Id, Prt, Src_Ip, Fragments, Last_Rcv) ->
    receive
	{add, Offset, Data, Mf} ->
	    New_Frgs = insert(Offset, Data, Fragments),
	    case test_completion(Mf, Last_Rcv, New_Frgs) of
		all_received ->
		    Packet = build_packet(New_Frgs),
		    ip:fragment(Frg_Id, Src_Ip, Prt, Packet),
		    ok;
		last_received ->
		    loop(Frg_Id, Prt, Src_Ip, New_Frgs, true);
		_ ->
		    loop(Frg_Id, Prt, Src_Ip, New_Frgs, false)
	    end;
	_ ->
	    loop(Frg_Id, Prt, Src_Ip, Fragments, Last_Rcv)
    end.

insert(Offset, Data, [{Elem_Offset, Elem_Data} | Tail]) ->
    if
	Offset == Elem_Offset -> % Repeated Segment
	    [{Elem_Offset, Elem_Data} | Tail];
	Offset < Elem_Offset ->
	    [{Offset, Data},{Elem_Offset, Elem_Data} | Tail];
	true ->
	    [{Elem_Offset, Elem_Data} | insert(Offset, Data, Tail)]
    end;
insert(Offset, Data, []) ->
    [{Offset, Data}].

test_completion(Mf, Last_Rcv, Fragments) ->    
if
    (Mf == 0) or (Last_Rcv) -> % Maybe
	case check_completed(Fragments) of
	    true ->
		all_received;
	    false ->
		last_received
	end;
    true ->
	missing_fragments
end.

check_completed([{0, Data} | Tail]) ->
    check_next(Tail, size(Data) +1);
check_completed(_) ->
    false.

check_next([], _) ->
    true;
check_next([{Offset, Data} | Tail], Offset) ->
    check_next(Tail, Offset+size(Data)+1);
check_next(_, _) ->
    false.

build_packet(Fragments) ->
    build_packet_1(Fragments, <<>>).

build_packet_1([], Packet) -> % Use a list here later to make it faster
    Packet;
build_packet_1([{_, Data} | Tail], Packet) ->
    build_packet_1(Tail, <<Packet/binary, Data/binary>>).
