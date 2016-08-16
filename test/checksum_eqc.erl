%%% @author Thomas Arts <thomas@ThomasComputer.local>
%%% @copyright (C) 2016, Quviq AB
%%% 
%%% @doc QuickCheck model for testing checksum.erl
%%% @end
%%% Created : 15 Aug 2016 by Thomas Arts <thomas.arts@quviq.com>
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
%%%--------------------------------------------------------------------------

-module(checksum_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

%% -- State ------------------------------------------------------------------
initial_state() ->
  init.

initial_state_data() ->
  [].

%% -- Transitions ------------------------------------------------------------
init() ->
  [ {running, start} ].

running() ->
  [ {running, check},
    {init, stop} ].

%% -- Operations -------------------------------------------------------------

start_args(init, running, _S) ->
  [].

start() ->
  checksum:start().

start_post(_, _, _S, [], _Res) ->
  case erl_ddll:loaded_drivers() of
    {ok, Drivers} ->
      lists:member("checksum", Drivers);
    Other ->
      Other
  end.
  

stop_args(running, init, _S) ->
  [].

stop() ->
  exit(whereis(checksum), kill),
  timer:sleep(2).

stop_post(_, _, _S, [], _Res) ->
  case erl_ddll:loaded_drivers() of
    {ok, Drivers} ->
      not lists:member("checksum", Drivers);
    Other ->
      Other
  end.


check_args(_From, _To, _S) ->
  [binary()].

check(Packet) ->
  checksum:checksum(Packet).

%% The C version of Checksum seems to be the same as the Erlang checksum_1
check_post(_From, _To, _S, [Packet], Sum) ->
  eq(Sum, checksum:checksum_1([Packet])).


weight(running, running, _Cmd, _) ->
  10;
weight(_, _, _Cmd, _) ->
  1.


prop_checksum() ->
  ?FORALL(Cmds, commands(?MODULE),
  begin
    catch stop(),
    {H, S, Res} = run_commands(?MODULE,Cmds),
    pretty_commands(?MODULE, Cmds, {H, S, Res},
                    aggregate(zip(state_names(H),command_names(Cmds)),
                              Res == ok))
  end).

