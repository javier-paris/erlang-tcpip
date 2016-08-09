%%%-------------------------------------------------------------------
%%% File    : rtt.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Round trip time and retransmition timeout estimation
%%%
%%% Created : 19 Nov 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(rtt).

-export([check_rttimer/7, set_rttimer/3]).

-define(DEFAULT_G, 1). % clock precision in ms
-define(MIN_RTO, 1000). % one second

%% Check if the packet used to measure rtt has been acked and update rto if so.
% Returns {Rttimer, Rtseq, Rto, Srtt, Rttvar}

check_rttimer(Rtseq, Rtcount, Rttimer, Srtt, Rttvar, Rto, Snd_Una) -> 
    if
	(Rtseq >= 0) and (Rtcount == 0) ->
	    case seq:lt(Rtseq, Snd_Una) of
		true ->
		    Rtt = timer:now_diff(erlang:timestamp(), Rttimer) / 1000,
		    {N_Rto, N_Srtt, N_Rttvar} = compute_rto(Srtt, Rttvar, Rtt),
		    {-1, -1, N_Rto, N_Srtt, N_Rttvar};
		false ->
		    {Rttimer, Rtseq, Rto, Srtt, Rttvar}
	    end;
	true ->
	    {Rttimer, Rtseq, Rto, Srtt, Rttvar}
    end.

%% Compute retransmit timeout from a new round trip time measure.
compute_rto(Srtt, Rttvar, Rtt) ->
    case Srtt of
	-1 -> % first time
	    N_Srtt = Rtt,
	    N_Rttvar = Rtt / 2,
	    Rto = max(N_Srtt + max(?DEFAULT_G, 4*N_Rttvar), ?MIN_RTO),
	    {Rto, N_Srtt, N_Rttvar};
	_ ->
	    N_Rttvar = 0.75*Rttvar + 0.25*abs(Srtt - Rtt),
	    N_Srtt = 0.875*Srtt + 0.125 * Rtt,
	    Rto = max(N_Srtt + max(?DEFAULT_G, 4*N_Rttvar), ?MIN_RTO),
	    {Rto, N_Srtt, N_Rttvar}
    end.

%% Check if we are alredy measuring rtt, and set a timer if not.
set_rttimer(Rttimer, Snd_Nxt, RtSeq) ->
    if 
	Rttimer == -1 ->
	    {erlang:timestamp(), Snd_Nxt};
	true ->
	    {Rttimer, RtSeq}
    end.
