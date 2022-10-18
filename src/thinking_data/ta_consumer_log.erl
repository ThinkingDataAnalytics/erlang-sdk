%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5月 2022 10:40
%%%-------------------------------------------------------------------
-module(ta_consumer_log).
-author("ThinkingData").

-include_lib("kernel/include/file.hrl").

%% API
-export([
  add/1,
  flush/0,
  close/0,
  init/0]).

-spec init() -> _.
init() ->
  lager:start().

-spec add(thinking_analytics_sdk:event()) -> _.
add(E) ->
  ta_logger:info(E).

%% 立即发送。暂时不需要调用
-spec flush() -> _.
flush() ->
  ok.

%% 关闭SDK
-spec close() -> _.
close() ->
  ok.
