%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5Month 2022 10:40
%%%-------------------------------------------------------------------
-module(ta_consumer_log).
-author("ThinkingData").
-include("thinking_engine_sdk.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([
  add/1,
  flush/0,
  close/0,
  init/0,
  init_with_logger/1,
  add_with_instance/2,
  flush_with_instance/1,
  close_with_instance/1
  ]).

-spec init() -> _.
init() ->
  lager:start().

-spec init_with_logger(fun()) -> _.
init_with_logger(Fun) ->
  lager:start(),
  #te_log_consumer{fun_log = Fun}.

-spec add(thinking_analytics_sdk:event()) -> _.
add(E) ->
  ta_logger:info(E).

-spec add_with_instance(#te_log_consumer{}, thinking_analytics_sdk:event()) -> _.
add_with_instance(Consumer, E) ->
  Fun = Consumer#te_log_consumer.fun_log,
  if
    is_function(Fun) -> Fun(E);
    true -> lager:info("TE write data faild. data: ~w~n", [E])
  end.

%% flush data immediately. don't need invoke.
-spec flush() -> _.
flush() ->
  ok.

%% flush data immediately. don't need invoke.
-spec flush_with_instance(#te_log_consumer{}) -> _.
flush_with_instance(Consumer) ->
  io:format("log consumer flush ~n"),
  Consumer,
  ok.

%% close SDK
-spec close() -> _.
close() ->
  ok.

%% flush data immediately. don't need invoke.
-spec close_with_instance(#te_log_consumer{}) -> _.
close_with_instance(Consumer) ->
  io:format("log consumer close ~n"),
  Consumer,
  ok.
