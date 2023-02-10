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

  Name = <<228,184,173,230,150,135,95,97,95,49,50,51>>,
  thinking_analytics_sdk:user_set("account_id_Erlang", "distinct_id", #{"id" => 12, "key_1" => [1,1,1,1], "key_2" => ["a", "b"], "key_3" => ["中", "文"], "key_4" => ["中文", "list"], "key_5" => "中文字符串", "amount" => 7.123, "nickName" => Name}),

  %% ordinary event
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"key_1" => "🚓🦽🦼🚲🚜🚜🦽", "key_2" => 2.2, "key_array" => ["🚌", "🏍", "😚😊"]}),

  %% nested properties event
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"unicode_key" => "🙂🆒😊"}]}]}),

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
  ok.

