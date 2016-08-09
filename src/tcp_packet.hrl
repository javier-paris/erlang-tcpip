%%%-------------------------------------------------------------------
%%% File    : tcp_packet.hrl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp Packet Info Structure
%%%
%%% Created : 17 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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


-record(pkt, {
	      sip,     % Source Ip
	      dip,     % Destination Ip
	      sport,   % Source Port
	      dport,   % Destination Port
	      seq,     % Sequence Number
	      ack,     % Acknowledge number
	      is_urg,  % Urgent flag
	      is_ack,  % Ack flag
	      is_psh,  % Push flag
	      is_rst,  % Reset flag
	      is_syn,  % Syn flag
	      is_fin,  % Fin flag
	      window,  % Window size
	      urgent,  % Urgent pointer
	      mss,     % Maximum segment size
	      data,    % Packet Data
	      data_size}). % Size of data
