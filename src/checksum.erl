%%%-------------------------------------------------------------------
%%% File    : checksum.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Ip Protocol Checksum
%%%
%%% Created :  4 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-module(checksum).

-export([checksum/1, checksum_1/1, start/0, init/1]).

-define(INT16MAX, 65535).

start() ->
    Ref = make_ref(),
    Self = self(),
    Pid = spawn(checksum, init, [{Self, Ref}]),
    receive
      {Ref, started} -> Pid
      after 5000 ->
          erlang:error("cannot start checksum driver")
    end.

init({Parent,Ref}) ->
    Path = filename:dirname(code:which(?MODULE)),
    ok = erl_ddll:load_driver(filename:join(Path,"../lib"), "checksum"),
    Port = open_port({spawn, checksum}, [binary]),
    register(checksum, self()),
    Parent ! {Ref, started},
    loop(Port).

checksum(Packet) ->
    checksum ! {check, Packet, self()},
    receive
	{checksum, Checksum} ->
	    Checksum
    end.

loop(Port) ->
    receive
	{check, Packet, From} ->
	    [Low, High | []] = port_control(Port, 0, Packet),
	    From ! {checksum, High*256+Low}
    end,
    loop(Port).

checksum_1(BinList) when is_list(BinList) ->
    checksum_1(BinList, 0);
checksum_1(Bin) ->
    (bnot checksum_2(Bin, 0)) band ?INT16MAX.

checksum_1([], Csum) ->
    (bnot Csum) band ?INT16MAX;
checksum_1([Bin|T], Csum) ->
    checksum_1(T, checksum_2(Bin, Csum)).

checksum_2(Bin = <<N1:16/integer, N2:16/integer, N3:16/integer, 
		  N4:16/integer, Rem/binary>>, Csum) when size(Bin) >= 8 ->
    checksum_2(Rem, Csum+N1+N2+N3+N4);
checksum_2(<<Num:16/integer, Remainder/binary>>, Csum) ->
    checksum_2(Remainder, Csum + Num);
checksum_2(<<Num:8/integer>>, Csum) ->
    checksum_2(<<>>, Csum+(Num bsl 8));
checksum_2(<<>>, Csum) when Csum > ?INT16MAX ->
    Carry = Csum bsr 16,
    checksum_2(<<>>, (Csum band ?INT16MAX) + Carry);
checksum_2(<<>>, Csum) ->
    Csum.
