%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5M 2022 10:27
%%%-------------------------------------------------------------------
-module(test_log_consumer).
-author("ThinkingData").

%% API
-export([test/0]).

test() ->
  %% init consumer
  ta_consumer_log:init(),
  %% init SDK with consumer
  thinking_analytics_sdk:init(thinking_analytics_sdk:consumer_type_log()),

  %% ordinary event
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"key_1" => "value_1", "key_2" => "value_2", "key_array" => ["a", "b", "c"]}),

  %% nested properties event
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"unicode_key" => "🙂🆒😊"}]}]}),

%%
%%  %% ⚠️ if your properties incloud time, please use ta_utils:format_time().
%%  %% incloud time
%%  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),
%%
%%  %% first event
%%  thinking_analytics_sdk:track_first("account_id_Erlang", "distinct_id", "first_login", "1", #{"key1" => "value1", "key2" => "value2"}),
%%  %% updatable event
%%  thinking_analytics_sdk:track_update("account_id_Erlang", "distinct_id", "ViewProduct", "event_id1", #{"key1" => "value1", "key2" => "value2"}),
%%  %% overwritable event
%%  thinking_analytics_sdk:track_overwrite("account_id_Erlang", "distinct_id", "ViewProduct", "event_id", #{"key1" => "value1", "key2" => "value2"}),
%%  %% user properties
%%  thinking_analytics_sdk:user_set("account_id_Erlang", "distinct_id", #{"age" => 18, "abc" => ["a", "b", "c"]}),
%%  thinking_analytics_sdk:user_add("account_id_Erlang", "distinct_id", #{"amount" => 100}),
%%  thinking_analytics_sdk:user_unset("account_id_Erlang", "distinct_id", ["age", "abc"]),
%%  thinking_analytics_sdk:user_set_once("account_id_Erlang", "distinct_id", #{"firstvalue" => 1}),
%%  thinking_analytics_sdk:user_append("account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4"]}),
%%  thinking_analytics_sdk:user_unique_append("account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4", "arr5"]}),
%%  thinking_analytics_sdk:user_del("account_id_Erlang", "distinct_id"),
  ok.

