%%%-------------------------------------------------------------------
%%% File    : congestion.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Congestion Control
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

-module(congestion).

-export([cgt_ctl/6]).

-define(max(X,Y), case X > Y of true -> X; false -> Y end).

% Updates congestion windows and slow start threshold
cgt_ctl(Rtcount, Snd_Nxt, Snd_Una, Smss, Cwnd, Ssthr) ->
    if
	Rtcount > 0 -> % There were retransmitions
	    FlightSize = seq:sub(Snd_Nxt, Snd_Una),
	    New_Ssthr = ?max(FlightSize/2, 2*Smss),
	    {Smss, New_Ssthr};
	true ->
	    {update_cwnd(Cwnd, Smss, Ssthr), Ssthr}
    end.

%% Updates the value of cwnd when a new ack arrives
update_cwnd(Cwnd, Smss, Ssthr) ->
    if
	Cwnd > Ssthr -> % Congestion avoidance
	    Cwnd + Smss*Smss/Cwnd;
	true -> % Slow start
	    Cwnd + Smss
    end.
