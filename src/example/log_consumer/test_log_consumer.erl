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
-export([test_v1/0, test_v2/0]).

%% single SDK instance API. Not recommend
test_v1() ->
  %% init consumer
  ta_consumer_log:init(),
  %% init SDK with consumer
  thinking_analytics_sdk:init(thinking_analytics_sdk:consumer_type_log()),

  %% ordinary event
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"key_1" => "🚓🦽🦼🚲🚜🚜🦽", "key_2" => 2.2, "key_array" => ["🚌", "🏍", "😚😊"]}),

  %% nested properties event
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"unicode_key" => "🙂🆒😊"}]}]}),

  %% ⚠️ if your properties include time, please use ta_utils:format_time().
  %% include time
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_id", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),

  %% first event
  thinking_analytics_sdk:track_first("account_id_Erlang", "distinct_id", "first_login", <<"1">>, #{"key1" => "value1", "key2" => "value2"}),
  %% updatable event
  thinking_analytics_sdk:track_update("account_id_Erlang", "distinct_id", "ViewProduct", "event_id1", #{"key1" => "value1", "key2" => "value2"}),
  %% overwrite event
  thinking_analytics_sdk:track_overwrite("account_id_Erlang", "distinct_id", "ViewProduct", "event_id", #{"key1" => "value1", "key2" => "value2"}),
  %% user properties
  Name = <<228,184,173,230,150,135,95,97,95,49,50,51>>,
  thinking_analytics_sdk:user_set("account_id_Erlang", "distinct_id", #{"id" => 12, "key_1" => [1,1,1,1], "key_2" => ["a", "b"], "key_3" => ["中", "文"], "key_4" => ["中文", "list"], "key_5" => "中文字符串", "amount" => 7.123, "nickName" => Name}),

  thinking_analytics_sdk:user_add("account_id_Erlang", "distinct_id", #{"amount" => 100}),
  thinking_analytics_sdk:user_unset("account_id_Erlang", "distinct_id", ["age", "abc"]),
  thinking_analytics_sdk:user_set_once("account_id_Erlang", "distinct_id", #{"firstValue" => 1}),
  thinking_analytics_sdk:user_append("account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4"]}),
  thinking_analytics_sdk:user_unique_append("account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4", "arr5"]}),
  thinking_analytics_sdk:user_del("account_id_Erlang", "distinct_id"),
  thinking_analytics_sdk:flush(),
  ok.

%% multiple SDK instance API. recommend
test_v2() ->
  %% init consumer
  %% A lager sink is provided by default: 'ta_logger'. You could add your own sink
  Consumer = ta_consumer_log:init_with_logger(fun(E) -> ta_logger:info(E) end),
  %% init SDK with consumer
  TE_SDK = thinking_analytics_sdk:init_with_consumer(Consumer),

  %% init consumer 1
  %% You could add your own sink. e.g. 'ta_logger_1'. it is must define in lager config file
  Consumer1 = ta_consumer_log:init_with_logger(fun(E) -> ta_logger:info(E) end),
  %% init SDK with consumer
  TE_SDK1 = thinking_analytics_sdk:init_with_consumer(Consumer1),

  ta_utils:for(0, 5, fun(_) ->
    thinking_analytics_sdk:track_instance(TE_SDK, "account_id_Erlang", "distinct_logbus", "ViewProduct_1", #{"key_1" => "🚓🦽🦼🚲🚜🚜🦽", "key_2" => 2.2, "key_array" => ["🚌", "🏍", "😚😊"]}),
    thinking_analytics_sdk:track_instance(TE_SDK1, "account_id_Erlang", "distinct_logbus", "ViewProduct_1", #{"key_1" => "🚓🦽🦼🚲🚜🚜🦽", "key_2" => 2.2, "key_array" => ["🚌", "🏍", "😚😊"]})
                     end
  ),

  %% ordinary event
  thinking_analytics_sdk:track_instance(TE_SDK, "account_id_Erlang", "distinct_logbus", "ViewProduct", #{"key_1" => "🚓🦽🦼🚲🚜🚜🦽", "key_2" => 2.2, "key_array" => ["🚌", "🏍", "😚😊"]}),
  %% nested properties event
  thinking_analytics_sdk:track_instance(TE_SDK, "account_id_Erlang", "distinct_logbus", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"unicode_key" => "🙂🆒😊"}]}]}),
  %% ⚠️ if your properties include time, please use ta_utils:format_time().
  %% include time
  thinking_analytics_sdk:track_instance(TE_SDK, "account_id_Erlang", "distinct_id", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),
  %% first event
  thinking_analytics_sdk:track_first_instance(TE_SDK1, "account_id_Erlang", "distinct_id", "first_login", <<"1">>, #{"key1" => "value1", "key2" => "value2"}),
  %% updatable event
  thinking_analytics_sdk:track_update_instance(TE_SDK1, "account_id_Erlang", "distinct_id", "ViewProduct", "event_id1", #{"key1" => "value1", "key2" => "value2"}),
  %% overwrite event
  thinking_analytics_sdk:track_overwrite_instance(TE_SDK1, "account_id_Erlang", "distinct_id", "ViewProduct", "event_id", #{"key1" => "value1", "key2" => "value2"}),

  %% user properties
  Name = <<228,184,173,230,150,135,95,97,95,49,50,51>>,
  thinking_analytics_sdk:user_set_instance(TE_SDK, "account_id_Erlang", "distinct_id", #{"id" => 12, "key_1" => [1,1,1,1], "key_2" => ["a", "b"], "key_3" => ["中", "文"], "key_4" => ["中文", "list"], "key_5" => "中文字符串", "amount" => 7.123, "nickName" => Name}),

  thinking_analytics_sdk:user_add_instance(TE_SDK, "account_id_Erlang", "distinct_id", #{"amount" => 100}),
  thinking_analytics_sdk:user_unset_instance(TE_SDK, "account_id_Erlang", "distinct_id", ["age", "abc"]),
  thinking_analytics_sdk:user_set_once_instance(TE_SDK1, "account_id_Erlang", "distinct_id", #{"firstValue" => 1}),
  thinking_analytics_sdk:user_append_instance(TE_SDK1, "account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4"]}),
  thinking_analytics_sdk:user_unique_append_instance(TE_SDK1, "account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4", "arr5"]}),
  thinking_analytics_sdk:user_del_instance(TE_SDK1, "account_id_Erlang", "distinct_id"),

  thinking_analytics_sdk:flush_instance(TE_SDK),
  thinking_analytics_sdk:flush_instance(TE_SDK1),

  ok.

