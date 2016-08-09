%%%-------------------------------------------------------------------
%%% File    : tcb.hrl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Transmision control block
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


-define(MAX_SND_BUFFER, 256000).
-define(MAX_RCV_BUFFER, 256000).
-define(DEFAULT_SSTHR, 32768).
-define(DEFAULT_RMSS, 1460).
-define(DEFAULT_SMSS, 536).
-define(DEFAULT_RTO, 2500).   % 2.5 seconds
-define(MAX_RTO, 60000). % one minute
-define(TCP_MAX_WINDOW, 65535).
-define(DEFAULT_MSL, 120000). % two minutes
-define(DEFAULT_DACK_TIME, 500). % half a second


-record(tcb, {
	  reader,                       % reader process
	  writer,                       % writer process
	  client,                       % client process

	  lc_port   = -1,               % local port number
	  rt_port   = -1,               % remote port number
	  lc_ip     = -1,               % local ip
	  rt_ip     = -1,               % remote ip

	  state,                        % connection state
	  maxsbsize = ?MAX_SND_BUFFER,  % maximum size of send buffer
	  sbuf      = queue:new(),      % send data buffer
	  sbsize    = 0,                % size of send buffer
	  maxrbsize = ?MAX_RCV_BUFFER,  % maximum size of receive buffer
	  rbsize    = 0,                % size of receive buffer
	  rbuf      = queue:new(),      % received data buffer

	  cwnd      = -1,               % congestion window size
	  ssthr     = ?DEFAULT_SSTHR,   % slow start threshold
	  smss      = ?DEFAULT_SMSS,    % send maximum segment size
	  rmss      = ?DEFAULT_RMSS,    % receive maximum segment size

	  rqueue    = queue:new(),      % retransmit queue
	  rtimer    = -1,               % retransmit timer
	  rto       = ?DEFAULT_RTO,     % retransmit timeout
	  rtcount   = 0,                % retransmit count
	  srtt      = -1,               % smothed round trip time estimation
	  rttvar    = -1,               % round trip time variance
	  rttimer   = -1,               % round trip timer
	  rtseq     = -1,               % sequence number for rtt measurement

	  snd_una,                      % first sent byte unacked
	  snd_nxt,                      % next byte to send
	  snd_wnd   = ?TCP_MAX_WINDOW,  % send window
	  snd_up    = 0,                % urgent pointer
	  snd_wl1   = 0,                % sequence number of last window update
	  snd_wl2   = 0,                % ack number of last window update
	  iss,                          % initial send sequence number

	  rcv_nxt   = -1,               % next sequence number to receive
	  rcv_wnd   = ?TCP_MAX_WINDOW,  % receive window
	  rcv_up    = -1,               % receive urgent pointer
	  irs       = -1,               % initial receive sequence number
	  twtimer   = -1,               % time-wait timer

	  send_fin  = 0,                % user closed but there is data left
	  out_order = out_order:new(),  % queue for out of order data
	  wrt_wait  = false,            % writer thread is on hold
	  dack_timer= -1,               % delayed ack timer
	  dack_data = 0,                % delayed ack data to be acked
	  
	  syn_queue = [],               % queue of opening connections for 
	                                %   listen sockets
	  open_queue= queue:new()       % queue of established connections for
	                                %   listen sockets
}).

