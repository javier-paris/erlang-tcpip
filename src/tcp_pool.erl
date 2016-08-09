%%%-------------------------------------------------------------------
%%% File    : tcp_pool.erl
%%% Author  : Javier Paris <javier.paris@udc.es>
%%% Description : Tcp Connection Pool
%%%
%%% Created : 13 Aug 2004 by Javier Paris <javier.paris@udc.es>
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

-module(tcp_pool).

-export([start/1,init/1,get/1,add/2,remove/1]).


%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%

start(Ip) ->
    spawn(tcp_pool, init, [Ip]).

get(Socket) ->
    tcp_pool ! {get, Socket, self()},
    receive
	{tcp_pool, ok, Socket, Conn} ->
	    {ok, Conn};
	{tcp_pool, error, no_connection} ->
	    {error, no_connection}
    end.

add({Type, Socket}, Conn) ->
    tcp_pool ! {add, Type, Socket, Conn, self()},
    receive 
	{tcp_pool, ok, Lc_Ip, Lc_Port} ->
	    {ok, Lc_Ip, Lc_Port};
	{tcp_pool, error, Error} ->
	    {error, Error}
    end.

remove(Socket) ->
    tcp_pool ! {remove, Socket}.


%%%%%%%%%%%%%%%%%%%%% Server Loop %%%%%%%%%%%%%%%%%%

init(Ip) ->
    register(tcp_pool, self()),
    ets:new(tcp_pool, [set, private, named_table]),
    loop(Ip).

loop(Ip) ->
    receive
	{get, Socket, From} ->
	    case catch ets:lookup_element(tcp_pool, Socket, 2) of
		{'EXIT', _} ->
		    From ! {tcp_pool, error, no_connection};
		Conn ->
		    From ! {tcp_pool, ok, Socket, Conn}
	    end;
	{add, remote, R_Socket, Conn, From} ->
	    {Rt_Ip, Rt_Port} = R_Socket,
	    Lc_Port = find_free_port(Ip, Rt_Ip, Rt_Port),
	    ets:insert(tcp_pool, {{Ip, Lc_Port, Rt_Ip, Rt_Port}, Conn}),
	    From ! {tcp_pool, ok, Ip, Lc_Port};
	{add, local, Lc_Port, Conn, From} ->
	    ets:insert(tcp_pool, {{Ip, Lc_Port}, Conn}),
	    From ! {tcp_pool, ok, Ip, Lc_Port};
	{add, connect, {Ip, Lc_Port, Rt_Ip, Rt_Port}, Conn, From} ->
	    ets:insert(tcp_pool, {{Ip, Lc_Port, Rt_Ip, Rt_Port}, Conn}),
	    From ! {tcp_pool, ok, Ip, Lc_Port};
	{remove, Socket} ->
	    ets:delete(tcp_pool, Socket)
    end,
    loop(Ip).

find_free_port(Lc_Ip, Rt_Ip, Rt_Port) ->
    find_free_port_1(Lc_Ip, Rt_Ip, Rt_Port, 1000).

find_free_port_1(Lc_Ip, Rt_Ip, Rt_Port, N) ->
    case ets:member(tcp_pool, {Lc_Ip, N, Rt_Ip, Rt_Port}) of
	true ->
	    find_free_port_1(Lc_Ip, Rt_Ip, Rt_Port, N+1);
	false ->
	    N
    end.
