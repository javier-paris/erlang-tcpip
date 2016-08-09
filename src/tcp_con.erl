%%%-------------------------------------------------------------------
%%% File    : tcp_con.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp Connection Management
%%%
%%% Created : 11 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(tcp_con).

-export([usr_open/2, usr_listen/1, usr_accept/1, init_reader/2, 
	 init_writer/1, recv/2, recv/3, usr_send/2, send_packet/2, 
	 close_connection/1, usr_recv/2, usr_close/1, new_mtu/2, 
	 dst_unr/1, clone/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API FOR APPLICATION LEVEL PROTOCOLS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

usr_open(Rt_Ip, Rt_Port) ->  % Active Open
    init(Rt_Ip, Rt_Port).

usr_listen(Lc_Port) ->  % Pasive Open
    init(Lc_Port).

usr_accept({Tcb, _Reader, _Writer}) ->
    accept(Tcb).

usr_close({Tcb, _Reader, Writer}) ->
    State = tcb:get_tcbdata(Tcb, state),
    case State:close() of
	ok ->
	    close(Tcb, Writer);
	{error, Error} ->
	    {error, Error}
    end.

usr_send({Tcb, _Reader, _Writer}, Data) ->
    State = tcb:get_tcbdata(Tcb, state),
    case State:queue() of
	ok ->
	    queue(Tcb, Data);
	{error, Error} ->
	    {error, Error}
    end.

usr_recv({Tcb, _Reader, _Writer}, Bytes) ->
    State = tcb:get_tcbdata(Tcb, state),
    case State:read(Tcb, Bytes) of
	{ok, New_Bytes} ->
	    read(Tcb, New_Bytes);
	{error, Error} ->
	    {error, Error}
    end.

%%%%%%%%%%%%%%%%%% API FOR OTHER TCP AND IP MODULES %%%%%%%%%%%%%

recv({_, Reader, _}, Pkt) ->
    Reader ! {recv, Pkt}.

recv(Src_Ip, {_, Reader, _}, Pkt) ->
    Reader ! {recv, Src_Ip, Pkt}.

send_packet(Writer, Type) ->
    Writer ! {event, {send, Type}}.

close_connection(Tcb) ->
    Reader = tcb:get_tcbdata(Tcb, reader),
    Writer = tcb:get_tcbdata(Tcb, writer),
    tcp_pool:remove({Tcb, Reader, Writer}),
    Reader ! close,
    Writer ! close,
    tcb:syncset_tcbdata(Tcb, state, closed),
    tcb:close(Tcb).

new_mtu({Tcb, _, _}, MTU) -> % For PMTU discovery.
    tcb:set_tcbdata(Tcb, smss, MTU-40).

dst_unr({_Tcb, _, _}) -> % Should send the user an error. Unimplemented
    ok.

%% Clone a connection using the data from this Tcb.
%% Used for creating connections from listen sockets
clone(Tcb) ->
    N_Tcb = tcb:clone(Tcb),
    Writer = spawn(tcp_con, init_writer, [N_Tcb]),
    Reader = spawn(tcp_con, init_reader, [N_Tcb, Writer]),
    tcb:set_tcbdata(N_Tcb, reader, Reader),
    tcb:set_tcbdata(N_Tcb, writer, Writer),
    {N_Tcb, Reader, Writer}.

%%%%%%%%%%%%%%% Reader and Writer loop %%%%%%%%%%%%%%%

%% Every other protocol has a process for sending and a second one for receiving. 
%% In tcp the processes are per connection

init(Rt_Ip, Rt_Port) ->
    Tcb = tcb:start(closed, Rt_Ip, Rt_Port),
    Writer = spawn_link(tcp_con, init_writer, [Tcb]),
    Reader = spawn_link(tcp_con, init_reader, [Tcb, Writer]),
    {ok, Lc_Ip, Lc_Port} = tcp_pool:add({remote, {Rt_Ip, Rt_Port}},
					{Tcb, Reader, Writer}),
    tcb:set_tcbdata(Tcb, lsocket, {Lc_Ip, Lc_Port}),
    send_packet(Writer, syn),
    wait_state(Tcb, [established]),
    {Tcb, Reader, Writer}.

init(Lc_Port) ->
    Tcb = tcb:start(listen),
    Writer = spawn(tcp_con, init_writer, [Tcb]),
    Reader = spawn(tcp_con, init_reader, [Tcb, Writer]),
    {ok, Lc_Ip, Lc_Port} = tcp_pool:add({local, Lc_Port}, 
					{Tcb, Reader, Writer}),
    tcb:set_tcbdata(Tcb, lsocket, {Lc_Ip, Lc_Port}),
%    wait_state(Tcb, [established]),
    {Tcb, Reader, Writer}.

init_reader(Tcb, Writer) ->
    tcb:set_tcbdata(Tcb, reader, self()),
    State = tcb:get_tcbdata(Tcb, state),
    reader_loop(Tcb, State, Writer).

init_writer(Tcb) ->
    tcb:set_tcbdata(Tcb, writer, self()),
    State = tcb:get_tcbdata(Tcb, state),
    writer_loop(Tcb, State, 0).

reader_loop(Tcb, State, Writer) ->
    receive
	{state, New_State} ->
	    reader_loop(Tcb, New_State, Writer)
    after 0 -> ok
    end,
    receive
	{state, N_State} ->
	    reader_loop(Tcb, N_State, Writer);
	{recv, Pkt} ->
	    State:recv(Tcb, Pkt, Writer),
	    reader_loop(Tcb, State, Writer);
	{'EXIT', normal} ->
	    ok;
	close ->
	    ok
    end.

writer_loop(Tcb, State, Data_Avail) ->
    receive
	{state, New_State} ->
	    writer_loop(Tcb, New_State, Data_Avail)
    after 0 -> ok
    end,
    {Timeout, Def_Msg} = check_send(Tcb, Data_Avail),
    receive
	{state, N_State} ->
	    writer_loop(Tcb, N_State, Data_Avail);
	{'EXIT', normal} ->
	    ok;
	close ->
	    ok;
	{event, Message} ->
	    New_Data_Avail = procces_msg(Tcb, State, Message),
	    writer_loop(Tcb, State, New_Data_Avail)
    after
	Timeout ->
	    New_Data_Avail = procces_msg(Tcb, State, {send, Def_Msg}),
	    writer_loop(Tcb, State, New_Data_Avail)
    end.

procces_msg(Tcb, State, Event) ->
    case State:send(Tcb, Event) of
	{ok, X} when X > 0 ->
	    X;
	_ ->
	    0
    end.
    

%% Check if there is something to be sent
check_send(Tcb, Data_Avail) ->
    case Data_Avail of
	0 ->
	    case tcb:get_tcbdata(Tcb, sbufsize) of
	    	0 ->
	    	    case tcb:get_tcbdata(Tcb, send_fin) of
			1 ->
		    	    {0, fin};
			0 ->
		    	    tcb:set_tcbdata(Tcb, writer_wait, self()), 
		    	    {infinity, data};
			timeout ->
		    	    exit(normal)
	    	    end;
		_ ->
		    tcb:set_tcbdata(Tcb, writer_wait, self()),
		    {infinity,data}
	    end;
	_ ->
	    {0, data}
    end.

%%%%%%%%%%%%%%%%%%%%% User Commands %%%%%%%%%%%%%%%%%%%%

wait_state(Tcb, State_List) ->
    tcb:subscribe(Tcb, state),
    wait_state_1(State_List),
    tcb:unsubscribe(Tcb, state).

wait_state_1(State_List) ->
    receive
	{state, State, _Who} ->
	    case lists:member(State, State_List) of
		true ->
		    ok;
		false ->
		    wait_state_1(State_List)
	    end
    end.


queue(Tcb, Data) ->
    tcb:subscribe(Tcb, sdata),
    tcb:set_tcbdata(Tcb, sdata, Data),
    queue_wait(),
    tcb:unsubscribe(Tcb, sdata),
    ok.

queue_wait() ->
    receive
	{sdata, Size, Bufsize} ->
	    if
		Size =< Bufsize ->
		    ok;
		true ->
		    queue_wait()
	    end
    end.

read(Tcb, Bytes) ->
    case tcb:get_tcbdata(Tcb, {rdata, Bytes}) of
	<<>> ->
	    tcb:subscribe(Tcb, rdata),
	    case tcb:get_tcbdata(Tcb, {rdata, Bytes}) of
		<<>> ->
		    receive
			{rdata, Size} when Size > 0 ->
			    tcb:unsubscribe(Tcb, rdata),
			    read(Tcb, Bytes)
		    end;
		Data ->
		    tcb:unsubscribe(Tcb, rdata),
		    Data
	    end;
	Data ->
	    Data
    end.

close(Tcb, Writer) ->
    tcp_con:send_packet(Writer, fin),
    wait_state(Tcb, [time_wait, closed]).

accept(Tcb) ->
    case tcb:get_tcbdata(Tcb, open_queue) of
	empty ->
	    tcb:subscribe(Tcb,open_queue),
	    receive
		open_con ->
		    tcb:unsubscribe(Tcb, open_queue),
		    accept(Tcb)
	    end;
	Socket ->
	    {Other_Tcb, _, _} = Socket,
	    link(Other_Tcb),
	    Socket
    end.
