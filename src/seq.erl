%%%-------------------------------------------------------------------
%%% File    : seq.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Sequence numbers operations
%%%
%%% Created : 18 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(seq).

-export([lt/2, le/2, gt/2, ge/2, add/2, sub/2, max/2, min/2]).

-define(INT32,4294967296).

lt(X, Y) ->
    ((X - Y) band 16#80000000) > 0.

le(X, Y) ->
    Z = X - Y,
    (Z == 0) orelse ((Z band 16#80000000) > 0).

gt(X, Y) ->
    not le(X, Y).

ge(X, Y) ->
    not lt(X, Y).

add(X, Y) ->
    (X + Y) rem ?INT32. 

sub(X, Y) ->
    (X - Y) rem ?INT32.

min(X, Y) ->
    case lt(X, Y) of
	true ->
	    X;
	_ ->
	    Y
    end.

max(X, Y) ->
    case lt(Y, X) of
	true ->
	    X;
	_ ->
	    Y
    end.
		  
