%%%-------------------------------------------------------------------
%%% File    : tcb.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcb process
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

-module(tcb).

-export([start/3, start/1, init/1, init/3, get_tcbdata/2, 
	 set_tcbdata/3, syncset_tcbdata/3, close/1, subscribe/2, 
	 unsubscribe/2, clone/1]).

-include("tcb.hrl").
-include("tcp_packet.hrl").

-define(min(X,Y), case X < Y of true -> X; false -> Y end).
-define(max(X,Y), case X > Y of true -> X; false -> Y end).

-record(observer, {state, rdata, sdata, open_queue}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(listen) ->
    spawn_link(tcb, init, [listen]).

start(closed, Rt_Ip, Rt_Port) ->
    spawn_link(tcb, init, [closed, Rt_Ip, Rt_Port]).

get_tcbdata(Tcb, Attr) ->
    Tcb ! {get, Attr, self()},
    receive
	{tcbdata, Data} ->
	    Data
    after 2000 ->
	    throw(timeout)
    end.

set_tcbdata(Tcb, Attr, Param) ->
    Tcb ! {set, Attr, Param, self()}.

syncset_tcbdata(Tcb, Attr, Param) ->
    Tcb ! {syncset, Attr, Param, self()},
    receive
	{syncset, ok} ->
	    ok
    after 2000 ->
	    throw(timeout)
    end.

clone(Tcb) ->
    Tcb ! {clone, self()},
    receive
	{clone, N_Tcb} ->
	    N_Tcb
    end.

subscribe(Tcb, Attr) ->
    Tcb ! {subscribe, Attr, self()},
    receive
	{tcb, ok} ->
	    ok
    end.

unsubscribe(Tcb, Attr) ->
    Tcb ! {unsubscribe, Attr, self()}.

close(Tcb) ->
    Tcb ! close.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(listen) ->
    Tcb = init_tcb(-1, -1, listen),
    Observers = init_observers(),
    loop(Tcb, Observers);
init(Tcb) ->
    loop(Tcb, init_observers()).

init(closed, Rt_Ip, Rt_Port) ->
    Tcb = init_tcb(Rt_Ip, Rt_Port, closed),
    Observers = init_observers(),
    loop(Tcb, Observers).

loop(Tcb, Observers) ->
    receive 
	{set, Param, Value, _From} ->
	    New_Tcb = set(Tcb, Observers, Param, Value),
	    loop(New_Tcb, Observers);
	{syncset, Param, Value, From} ->
	    New_Tcb = set(Tcb, Observers, Param, Value),
	    From ! {syncset, ok},
	    loop(New_Tcb, Observers);
	{get, Param, From} ->
	    New_Tcb = get(Tcb, Observers, Param, From),
	    loop(New_Tcb, Observers);
	{subscribe, Param, From} ->
	    From ! {tcb,ok},
	    loop(Tcb, add(Param, From, Observers));
	{unsubscribe, Param, From} ->
	    loop(Tcb, remove(Param, From, Observers));
	close ->
	    cancel_timers(Tcb),
	    set(Tcb, Observers, state, closed),
	    exit(Tcb#tcb.writer, normal),
	    exit(Tcb#tcb.reader, normal);
	{state, established, Socket} ->
	    {Other_Tcb, _, _} = Socket,
	    tcb:unsubscribe(Other_Tcb, state),
	    New_Tcb = set(Tcb, Observers, open_queue, Socket),
	    loop(New_Tcb, Observers);
	{state, _State, _} -> % Erase them if the connection closes??
	    loop(Tcb, Observers);
	{clone, From} ->
	    N_Tcb=Tcb#tcb{reader=-1,
			  writer=-1,
			  syn_queue=[],
			  open_queue=queue:new()},
	    NTcb_Proc = spawn(tcb, init, [N_Tcb]),
	    From ! {clone, NTcb_Proc},
	    loop(Tcb, Observers)
    end.


%%%%%%%%%%%%%%%%%%%%%%  GET DATA FUNCTIONS %%%%%%%%%%%%%%%%%%%
get(Tcb, _, reader, From) ->
    From ! {tcbdata, Tcb#tcb.reader},
    Tcb;
get(Tcb, _, writer, From) ->
    From ! {tcbdata, Tcb#tcb.writer},
    Tcb;
get(Tcb, _, socket, From) ->
    
    From ! {tcbdata, {Tcb#tcb.lc_ip, Tcb#tcb.lc_port,
		      Tcb#tcb.rt_ip, Tcb#tcb.rt_port,
		      Tcb#tcb.rcv_nxt, Tcb#tcb.rcv_wnd}},
    Tcb;
get(Tcb, _, state, From) ->
    From ! {tcbdata, Tcb#tcb.state},
    Tcb;
get(Tcb, Observers, sdata, From) ->
    Size = get_data_size(Tcb),
    if
	Size > 0 ->
	    {Data, Rem} = get_data(Tcb#tcb.sbuf, Size, <<>>),
	    if
		Tcb#tcb.dack_timer == -1 ->
		    ok;
		true ->
		    erlang:cancel_timer(Tcb#tcb.dack_timer)
	    end,
	    New_Tcb = Tcb#tcb{sbuf       = Rem,
			      sbsize     = (Tcb#tcb.sbsize - size(Data)),
			      snd_nxt    = seq:add(Tcb#tcb.snd_nxt, size(Data)),
			      dack_timer = -1,
			      dack_data  = 0},
	    Data_Avail = get_data_size(New_Tcb),
	    notify(New_Tcb, Observers, sdata);
       true ->
	    Data = <<>>,
	    New_Tcb = Tcb,
	    Data_Avail = 0
    end,
    From ! {tcbdata, {Tcb#tcb.snd_nxt, Data_Avail, Data, size(Data),
		      Tcb#tcb.lc_ip, Tcb#tcb.lc_port,
		      Tcb#tcb.rt_ip, Tcb#tcb.rt_port,
		      Tcb#tcb.rcv_nxt, Tcb#tcb.rcv_wnd}},
    New_Tcb;
get(Tcb, _, sbufsize, From) ->
    From ! {tcbdata, Tcb#tcb.sbsize},
    Tcb;
get(Tcb, _, maxsbufsize, From) ->
    From ! {tcbdata, Tcb#tcb.maxsbsize},
    Tcb;
get(Tcb, _, data_available, From) ->
    From ! {tcbdata, get_data_size(Tcb)},
    Tcb;
get(Tcb, _, {rdata, Bytes}, From) ->
    Size = ?min(Bytes, Tcb#tcb.rbsize),
    {Data, Rem} = get_data(Tcb#tcb.rbuf, Size, <<>>),
    New_Size = Tcb#tcb.rbsize - Size,
    Free_Buf = ?max(Tcb#tcb.maxrbsize-New_Size, 0),
    Rcv_Wnd = ?min(?TCP_MAX_WINDOW, Free_Buf),

    From ! {tcbdata, Data},
    Tcb#tcb{rbuf=Rem, rbsize=New_Size, rcv_wnd = Rcv_Wnd};
get(Tcb, _, rbufsize, From) ->
    From ! {tcbdata, Tcb#tcb.rbsize},
    Tcb;
get(Tcb, _, maxrbufsize, From) ->
    From ! {tcbdata, Tcb#tcb.maxrbsize},
    Tcb;
get(Tcb, _, cw_ss, From) ->
    From ! {tcbdata, {Tcb#tcb.cwnd, Tcb#tcb.ssthr}},
    Tcb; 
get(Tcb, _, rmss, From) ->
    From ! {tcbdata, Tcb#tcb.rmss},
    Tcb;
get(Tcb, _, smss, From) ->
    From ! {tcbdata, Tcb#tcb.smss},
    Tcb;
get(Tcb, _, rqueue, From) ->
    case catch queue:last(Tcb#tcb.rqueue) of
	{'EXIT', _} ->
	    From ! {tcbdata, empty},
	    Tcb;
	{_, Packet} ->
	    From ! {tcbdata, Packet},
	    if Packet#pkt.data_size > Tcb#tcb.smss -> % PMTU. Do not increase.
		    Tcb;
	       true ->
		    Rto = round(?min(?MAX_RTO, Tcb#tcb.rto * 2)),
		    Rtcount = Tcb#tcb.rtcount + 1,
		    Timer = erlang:send_after(Rto, Tcb#tcb.writer, {event, rto}),
		    Tcb#tcb{rto = Rto, rtcount = Rtcount, 
			    rtimer = Timer, rttimer = -1, rtseq = -1}
	    end
    end;
get(Tcb, _, snd, From) ->
    From ! {tcbdata, {Tcb#tcb.snd_una, Tcb#tcb.snd_nxt,
		      Tcb#tcb.snd_wnd, Tcb#tcb.snd_up}},
	Tcb;
get(Tcb, _, rto, From) ->
    From ! {tcbdata, {Tcb#tcb.rto, Tcb#tcb.rtcount,
		      Tcb#tcb.srtt, Tcb#tcb.rttvar}},
    Tcb;
get(Tcb, _, sndwl, From) ->
    From ! {tcbdata, {Tcb#tcb.snd_wl1, Tcb#tcb.snd_wl2}},
    Tcb;
get(Tcb, _, iss, From) ->
    From ! {tcbdata, Tcb#tcb.iss},
    Tcb;
get(Tcb, _, rcv, From) ->
    From ! {tcbdata, {Tcb#tcb.rcv_nxt, Tcb#tcb.rcv_wnd,
		      Tcb#tcb.rcv_up}},
    Tcb;
get(Tcb, _, irs, From) ->
    From ! {tcbdata, Tcb#tcb.irs},
    Tcb;
get(Tcb, _, send_fin, From) ->
    From ! {tcbdata, Tcb#tcb.send_fin},
    Tcb#tcb{send_fin=0};
get(Tcb, _, out_order, From) ->
    {Element, New_List} = out_order:get_out_order(Tcb#tcb.out_order, 
						  Tcb#tcb.rcv_nxt),
    From ! {tcbdata, Element},
    Tcb#tcb{out_order = New_List};
get(Tcb, _, open_queue, From) ->
    case queue:out_r(Tcb#tcb.open_queue) of
	{empty, _} ->
	    From ! {tcbdata, empty},
	    Tcb;
	{{value, Socket}, Q2} ->
	    From ! {tcbdata, Socket},
	    Tcb#tcb{open_queue = Q2}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%% Set Data Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%

set(Tcb, _, reader, Reader) ->
    Tcb#tcb{reader = Reader};
set(Tcb, _, writer, Writer) ->
    Tcb#tcb{writer = Writer};
set(Tcb, _, rsocket, Socket) ->
    {Rt_ip, Rt_port} = Socket,
    Tcb#tcb{rt_ip = Rt_ip, rt_port = Rt_port};
set(Tcb, _, lsocket, Socket) ->
    {Lc_Ip, Lc_Port} = Socket,
    Tcb#tcb{lc_ip = Lc_Ip, lc_port = Lc_Port};
set(Tcb, Observers, state, State) ->
    New_Tcb = Tcb#tcb{state = State},
    notify(New_Tcb, Observers, state),
    New_Tcb;
set(Tcb, Observers, sdata, Data) ->
    New_Data = queue:cons(Data, Tcb#tcb.sbuf),
    New_Size = Tcb#tcb.sbsize + size(Data),
    New_Tcb = Tcb#tcb{sbuf = New_Data, sbsize = New_Size},
    case get_data_size(New_Tcb) of
	0 ->
	    ok;
	_ ->
	    if Tcb#tcb.wrt_wait ->
	    	tcp_con:send_packet(Tcb#tcb.writer, data);
	    true -> 
	    	ok
	    end
    end,
    notify(New_Tcb, Observers, sdata),
    New_Tcb;
set(Tcb, Observers, rdata, Data) ->
    New_Data = queue:cons(Data, Tcb#tcb.rbuf),
    New_Size = Tcb#tcb.rbsize + size(Data),
    
    Rcv_Nxt = seq:add(Tcb#tcb.rcv_nxt, size(Data)),
    Free_Buf = ?max(Tcb#tcb.maxrbsize-New_Size, 0),
    Rcv_Wnd = ?min(?TCP_MAX_WINDOW, Free_Buf),
    
    New_Tcb = Tcb#tcb{rbuf = New_Data, rbsize = New_Size,
		      rcv_nxt = Rcv_Nxt, rcv_wnd = Rcv_Wnd},
    notify(New_Tcb, Observers, rdata),
    New_Tcb;
set(Tcb, _, cwnd, Cwnd) ->
    Tcb#tcb{cwnd = Cwnd};
set(Tcb, _, ssthr, Ssthr) ->
    Tcb#tcb{ssthr = Ssthr};
set(Tcb, _, rmss, Mss) ->
    Tcb#tcb{rmss = Mss};
set(Tcb, _, smss, Mss) -> % Either Starting, or PMTU. Force a retransmit.
    case catch queue:last(Tcb#tcb.rqueue) of
	{'EXIT', _} ->
	    ok;
	{_, Packet} ->
	    if Packet#pkt.data_size > Mss ->
		    tcp_con:send_packet(Tcb#tcb.writer, rto);
	       true ->
		    ok
	    end
    end,
    Tcb#tcb{smss = Mss};
set(Tcb, _, rqueue, Data) ->
    Timer = set_rtimer(Tcb, Tcb#tcb.writer),
    Q = queue:cons(Data, Tcb#tcb.rqueue),
    Tcb#tcb{rqueue= Q, rtimer = Timer};
set(Tcb, _, rto, {Rto, Srtt, Rttvar}) ->
    Tcb#tcb{rto = Rto, srtt = Srtt, rttvar = Rttvar};
set(Tcb, _, rtcount, Rttcount) ->
    Tcb#tcb{rtcount = Rttcount};
set(Tcb, _, snd_una, Snd_Una) ->
    {Queue, Timer} = update_rqueue(Tcb, Snd_Una),
    {Cwnd, Ssthr} = congestion:cgt_ctl(Tcb#tcb.rtcount, Tcb#tcb.snd_nxt,
				         Tcb#tcb.snd_una, Tcb#tcb.smss,
				         Tcb#tcb.cwnd,    Tcb#tcb.ssthr),
    {Rttimer, RtSeq, Rto, Srtt, Rttvar} = 
	rtt:check_rttimer(Tcb#tcb.rtseq, Tcb#tcb.rtcount, Tcb#tcb.rttimer, 
			  Tcb#tcb.srtt, Tcb#tcb.rttvar, Tcb#tcb.rto, Snd_Una),
    Tcb#tcb{snd_una = Snd_Una, rqueue = Queue, rtimer=Timer,
	    rtcount = 0, cwnd = Cwnd, ssthr = Ssthr, rttimer = Rttimer, 
	    rtseq = RtSeq, rto = Rto, srtt = Srtt, rttvar = Rttvar};
set(Tcb, _, snd_nxt, Inc) ->
    {Timer, Seq} = rtt:set_rttimer(Tcb#tcb.rttimer, Tcb#tcb.snd_nxt, 
				   Tcb#tcb.rtseq),
    Tcb#tcb{snd_nxt = seq:add(Tcb#tcb.snd_nxt, Inc),
	    rttimer = Timer, rtseq = Seq};
set(Tcb, _, snd_wnd, {Wnd, Seq, Ack}) ->
    case seq:lt(Tcb#tcb.snd_wl1, Seq) orelse
	((Tcb#tcb.snd_wl1 == Seq) andalso seq:le(Tcb#tcb.snd_wl2, Ack)) of
	true ->
	    New_Tcb = Tcb#tcb{snd_wnd = Wnd, snd_wl1 = Seq, snd_wl2= Ack},
	    if
		New_Tcb#tcb.wrt_wait  ->
		    case get_data_size(New_Tcb) of
			0 ->
			    New_Tcb;
			_ ->
			    tcp_con:send_packet(Tcb#tcb.writer, data),
			    New_Tcb#tcb{wrt_wait=false}
		    end;
		true ->
		    New_Tcb
	    end;
	false ->
	    Tcb
    end;
set(Tcb, _, rcv_nxt, Nxt) ->
    Tcb#tcb{rcv_nxt = Nxt};
set(Tcb, _, rcv_wnd, Wnd) ->
    Tcb#tcb{rcv_wnd = Wnd};
set(Tcb, _, irs, Irs) ->
    Tcb#tcb{irs = Irs, snd_wl1 = Irs, snd_wl2 = Tcb#tcb.snd_nxt};
set(Tcb, _, send_fin, Send_Fin) ->
    Tcb#tcb{send_fin = Send_Fin};
set(Tcb, _, twtimer, Proc) ->
    catch erlang:cancel_timer(Tcb#tcb.twtimer),
    Timer = erlang:send_after(?DEFAULT_MSL*2, Proc, time_wait),
    Tcb#tcb{twtimer = Timer};
set(Tcb, _, out_order, Data) ->
    New_List = out_order:merge_data(Tcb#tcb.out_order, Data),
    Tcb#tcb{out_order = New_List};
set(Tcb, _, writer_wait, _) ->
    case get_data_size(Tcb) of
	0 ->
	    Tcb#tcb{wrt_wait = true};
	_ ->
	    tcp_con:send_packet(Tcb#tcb.writer, data),
	    Tcb
    end;
set(Tcb, _, del_ack, Size) ->
    case Tcb#tcb.dack_timer of
	    -1 ->
		Timer = erlang:send_after(?DEFAULT_DACK_TIME, Tcb#tcb.writer, {event, {send, ack}}),
		io:format("Setting ack Timer for ~w ~w ~w~n",[?DEFAULT_DACK_TIME, Tcb#tcb.writer, {send, ack}]),
		Tcb#tcb{dack_timer = Timer, dack_data = Size};
	Timer ->
	    New_Size = Tcb#tcb.dack_data + Size,
	    if 
		New_Size >= (2*Tcb#tcb.rmss) ->
		    tcp_con:send_packet(Tcb#tcb.writer, ack),
		    catch erlang:cancel_timer(Timer),
		    Tcb#tcb{dack_timer = -1, dack_data = 0};
		true ->
		    Tcb#tcb{dack_data = New_Size}
	    end
    end;
set(Tcb, _, syn_queue, Socket) ->
    {Other_Tcb, _, _} = Socket,
    tcb:subscribe(Other_Tcb, state),
    Tcb#tcb{syn_queue = [Socket | Tcb#tcb.syn_queue]};
set(Tcb, Observers, open_queue, Socket) ->
    N_syn_queue = lists:filter(fun (X) -> if X==Socket -> false; 
					     true -> true end end,
			       Tcb#tcb.syn_queue),
	notify(Tcb, Observers, open_queue),
    Tcb#tcb{syn_queue = N_syn_queue, 
	    open_queue = queue:cons(Socket, Tcb#tcb.open_queue)}.
    

%%%%%%%%%%%%%%%%%%%%% Observer add and remove %%%%%%%%%%%%%%%%%%%%%%%

add(rdata, From, Observers) ->
    Observers#observer{rdata=From};
add(sdata, From, Observers) ->
    Observers#observer{sdata=From};
add(state, From, Observers) ->
    Observers#observer{state=From};
add(open_queue, From, Observers) ->
    Observers#observer{open_queue=From}.

remove(rdata, _, Observers) ->
    Observers#observer{rdata=false};
remove(sdata, _, Observers) ->
    Observers#observer{sdata=false};
remove(state, _, Observers) ->
    Observers#observer{state=false};
remove(open_queue, _, Observers) ->
    Observers#observer{open_queue=false}.

notify(Tcb, Observers, rdata) ->
    catch Observers#observer.rdata ! {rdata, Tcb#tcb.rbsize};
notify(Tcb, Observers, sdata) ->
    catch Observers#observer.sdata ! {sdata, Tcb#tcb.sbsize,
				      Tcb#tcb.maxsbsize};
notify(Tcb, Observers, state) ->
    Tcb#tcb.reader ! {state, Tcb#tcb.state},
    Tcb#tcb.writer ! {state, Tcb#tcb.state},
    Socket = {self(), Tcb#tcb.reader, Tcb#tcb.writer},
    catch Observers#observer.state ! {state, Tcb#tcb.state, Socket};
notify(_Tcb, Observers, open_queue) ->
    catch Observers#observer.open_queue ! open_con.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_observers() ->
    #observer{
	  rdata = [],
	  sdata = [],
	  state = []
	}.

init_tcb(Rt_Ip, Rt_Port, State) ->
    Iss = iss:get_iss(),
    #tcb{
	  rt_port = Rt_Port,
	  rt_ip   = Rt_Ip,
	  state   = State,
	  snd_una = Iss,
	  snd_nxt = Iss,
	  iss     = Iss
	}.

get_data(Buf, Size, Acc) ->
    case catch queue:last(Buf) of
	{'EXIT', _} ->
	    {Acc, Buf};
	Data ->
	    if size(Data) > Size ->
		    <<Partial_Data:Size/binary, Rem/binary>> = Data,
		    {list_to_binary(lists:flatten([Acc, Partial_Data])),
		     queue:snoc((queue:init(Buf)), Rem)};
	       size(Data) == Size ->
		    {list_to_binary(lists:flatten([Acc, Data])), 
		     queue:init(Buf)};
	       true ->
		    get_data(queue:init(Buf), Size - size(Data), 
			     [Acc, Data])
	    end
    end.

get_available_window(Tcb) ->
    ?min(Tcb#tcb.snd_wnd, round(Tcb#tcb.cwnd)) -
   	seq:sub(Tcb#tcb.snd_nxt, Tcb#tcb.snd_una).

get_data_size(Tcb) ->
    ?max(0, ?min(get_available_window(Tcb),
	       ?min(Tcb#tcb.smss, Tcb#tcb.sbsize))).

set_rtimer(Tcb, From) ->
    case queue:is_empty(Tcb#tcb.rqueue) of
	 true ->
	     erlang:send_after(round(Tcb#tcb.rto), From, rto);
	 false ->
	     Tcb#tcb.rtimer
    end.

update_rqueue(Tcb, Snd_Una) ->
    update_rqueue_1(Tcb, Tcb#tcb.rqueue, Snd_Una).

update_rqueue_1(Tcb, Queue, Snd_Una) ->
    case catch queue:last(Queue) of
	{'EXIT', _} -> % Queue empty => Everything acked. Turn off timer
	    catch erlang:cancel_timer(Tcb#tcb.rtimer),
	    {Queue, ok};
	{Last_Seq, _} ->
	    case seq:le(Last_Seq, Snd_Una) of
		true -> % Unqueue Packet
		    update_rqueue_1(Tcb, queue:init(Queue), Snd_Una);
		false -> % This packet is not acked -> Restart timer
		    erlang:cancel_timer(Tcb#tcb.rtimer),
		    Rto = round(Tcb#tcb.rto),
		    Timer = erlang:send_after(Rto, Tcb#tcb.writer, {event, rto}),
		    {Queue, Timer}
	    end
    end.

% Cancels all the timers
cancel_timers(Tcb) ->
    catch erlang:cancel_timer(Tcb#tcb.rtimer),
    catch erlang:cancel_timer(Tcb#tcb.twtimer).

