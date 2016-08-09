%%%-------------------------------------------------------------------
%%% File    : iss.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Initial Segment number generator
%%%
%%% Created : 12 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(iss).

-export([start/0,init/0,get_iss/0]).

-define(TCP_MAX_ISS, 4294967295).
-define(TCP_ISS_INC, 64000).

%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    spawn(iss, init, []).

get_iss() ->
    iss ! {get_iss, self()}, 
    receive
	{iss, Iss} ->
	    Iss
    end.

%%%%%%%%%%%%%%%%%%%%%%% Iss Server %%%%%%%%%%%%%%%%%%%%%%%%

init() ->
    {_, Secs, Msecs} = erlang:timestamp(),
    Iss = Secs*Msecs rem ?TCP_MAX_ISS,
    register(iss, self()),
    loop(Iss).

loop(Iss) ->
    receive
	{get_iss, From} ->
	    From ! {iss, Iss},
	    loop((Iss + ?TCP_ISS_INC) rem ?TCP_MAX_ISS);
	_ ->
	    loop(Iss)
    end.
