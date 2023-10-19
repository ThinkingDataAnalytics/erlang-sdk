%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Write data to file
%%% @end
%%% Created : 16. 5Month 2022 10:40
%%%-------------------------------------------------------------------
-module(td_log_consumer).
-author("ThinkingData").
-include("td_analytics.hrl").
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

%% @doc Init log consumer
-spec init() -> _.
init() ->
  lager:start().

%% @doc (API_v2) Init log consumer with custom logger function
-spec init_with_logger(fun()) -> _.
init_with_logger(Fun) ->
  lager:start(),
  #td_log_consumer{fun_log = Fun}.

%% @doc Add event
-spec add(td_analytics:event()) -> _.
add(E) ->
  ta_logger:info(E).

%% @doc (API_v2) Add event
-spec add_with_instance(td_analytics:td_log_consumer(), thinking_analytics_sdk:event()) -> _.
add_with_instance(Consumer, E) ->
  Fun = Consumer#td_log_consumer.fun_log,
  if
    is_function(Fun) -> Fun(E);
    true -> lager:info("TE write data faild. data: ~w~n", [E])
  end.

%% @doc Flush data immediately. don't need invoke.
-spec flush() -> _.
flush() ->
  ok.

%% @doc (API_v2) Flush data immediately. don't need invoke.
-spec flush_with_instance(td_analytics:td_log_consumer()) -> _.
flush_with_instance(Consumer) ->
  io:format("log consumer flush ~n"),
  Consumer,
  ok.

%% @doc Close SDK
-spec close() -> _.
close() ->
  ok.

%% @doc (API_v2) Close data immediately. don't need invoke.
-spec close_with_instance(#td_log_consumer{}) -> _.
close_with_instance(Consumer) ->
  io:format("log consumer close ~n"),
  Consumer,
  ok.
