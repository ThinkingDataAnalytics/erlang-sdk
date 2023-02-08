%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5M 2022 10:28
%%%-------------------------------------------------------------------
-module(test_debug_batch_consumer).
-author("ThinkingData").

%% API
-export([test/0]).

test() ->
  %% init consumer
  ta_consumer_debug:init(),
  %% config server_url
  ta_consumer_debug:config_server_url("server_url"),
  %% config app_id
  ta_consumer_debug:config_app_id("app_id"),
  %% config device_id，be used to debug in TE Debug Model
  ta_consumer_debug:config_device_id("device_id"),
  %% config is need to archive
  ta_consumer_debug:config_is_write(true),

  %% init SDK with Consumer
  thinking_analytics_sdk:init(thinking_analytics_sdk:consumer_type_debug()),

  %% ordinary event
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"key_1" => "value_1", "key_2" => "value_2", "key_array" => ["a", "b", "c"]}),

  %% nested properties event
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"unicode_key" => "🙂🆒😊"}]}]}),

  %% ordinary event
%%  thinking_analytics_sdk:track("account_id_Erlang", "distinct_id", "ViewProduct", #{"key_1" => "value_1", "key_2" => "value_2"}),
%%  %% nested properties event
%%  thinking_analytics_sdk:track("account_id_Erlang", "distinct_id", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"child_key" => "child_value"}]}]}),
%%
%%  %% ⚠️ if your properties include time, please use ta_utils:format_time().
%%  %% include time
%%  thinking_analytics_sdk:track("account_id_Erlang", "distinct_id", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),
%%
%%  %% first event
%%  thinking_analytics_sdk:track_first("account_id_Erlang", "distinct_id", "first_login", "1", #{"key1" => "value1", "key2" => "value2"}),
%%  %% updatable event
%%  thinking_analytics_sdk:track_update("account_id_Erlang", "distinct_id", "ViewProduct", "event_id1", #{"key1" => "value1", "key2" => "value2"}),
%%  %% overwrite event
%%  thinking_analytics_sdk:track_overwrite("account_id_Erlang", "distinct_id", "ViewProduct", "event_id", #{"key1" => "value1", "key2" => "value2"}),
%%  %% user properties
%%  thinking_analytics_sdk:user_set("account_id_Erlang", "distinct_id", #{"age" => 18, "abc" => ["a", "b", "c"]}),
%%  thinking_analytics_sdk:user_add("account_id_Erlang", "distinct_id", #{"amount" => 100}),
%%  thinking_analytics_sdk:user_unset("account_id_Erlang", "distinct_id", ["age", "abc"]),
%%  thinking_analytics_sdk:user_set_once("account_id_Erlang", "distinct_id", #{"firstValue" => 1}),
%%  thinking_analytics_sdk:user_append("account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4"]}),
%%  thinking_analytics_sdk:user_unique_append("account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4", "arr5"]}),
%%  thinking_analytics_sdk:user_del("account_id_Erlang", "distinct_id"),

  %% close SDK
  thinking_analytics_sdk:close().


