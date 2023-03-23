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
-export([test_v1/0, test_v2/0]).

%% single SDK instance API. Not recommend
test_v1() ->
  ServerUrl = "server_url",
  AppID = "app_id",
  IsWrite = true,
  DeviceId = "123456789",

  %% init consumer
  ta_consumer_debug:init(),
  %% config server_url
  ta_consumer_debug:config_server_url(ServerUrl),
  %% config app_id
  ta_consumer_debug:config_app_id(AppID),
  %% config device_id，be used to debug in TE Debug Model
  ta_consumer_debug:config_device_id(DeviceId),
  %% config is need to archive
  ta_consumer_debug:config_is_write(IsWrite),

  %% init SDK with Consumer
  thinking_analytics_sdk:init(thinking_analytics_sdk:consumer_type_debug()),

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
  %% instance 0
  ServerUrl = "server_url",
  AppID = "app_id",
  IsWrite = true,
  DeviceId = "123456789",
  %% init consumer
  Consumer = ta_consumer_debug:init_with_config(ServerUrl, AppID, IsWrite, DeviceId),
  %% init SDK with consumer
  TE_SDK = thinking_analytics_sdk:init_with_consumer(Consumer),

  %% instance 1
  ServerUrl1 = "server_url",
  AppID1 = "app_id",
  IsWrite1 = true,
  DeviceId1 = "123456789",
  %% init consumer
  Consumer1 = ta_consumer_debug:init_with_config(ServerUrl1, AppID1, IsWrite1, DeviceId1),
  %% init SDK with consumer
  TE_SDK1 = thinking_analytics_sdk:init_with_consumer(Consumer1),

  AccountId0 = "account_id_Erlang_0",
  AccountId1 = "account_id_Erlang_1",

  ta_utils:for(0, 5, fun(_) ->
    thinking_analytics_sdk:track_instance(TE_SDK, AccountId0, "distinct_logbus", "ViewProduct_1", #{"key_1" => "🚓🦽🦼🚲🚜🚜🦽", "key_2" => 2.2, "key_array" => ["🚌", "🏍", "😚😊"]}),
    thinking_analytics_sdk:track_instance(TE_SDK1, AccountId1, "distinct_logbus", "ViewProduct_1", #{"key_1" => "🚓🦽🦼🚲🚜🚜🦽", "key_2" => 2.2, "key_array" => ["🚌", "🏍", "😚😊"]})
               end
  ),

  %% ordinary event
  thinking_analytics_sdk:track_instance(TE_SDK, AccountId0, "distinct_logbus", "ViewProduct", #{"#key_1" => "🚓🦽🦼🚲🚜🚜🦽", "key_2" => 2.2, "key_array" => ["🚌", "🏍", "😚😊"]}),
  %% nested properties event
  thinking_analytics_sdk:track_instance(TE_SDK, AccountId0, "distinct_logbus", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"unicode_key" => "🙂🆒😊"}]}]}),
  %% ⚠️ if your properties include time, please use ta_utils:format_time().
  %% include time
  thinking_analytics_sdk:track_instance(TE_SDK, AccountId0, "distinct_id", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),
  %% first event
  thinking_analytics_sdk:track_first_instance(TE_SDK1, AccountId1, "distinct_id", "first_login", <<"1">>, #{"key1" => "value1", "key2" => "value2"}),
  %% updatable event
  thinking_analytics_sdk:track_update_instance(TE_SDK1, AccountId1, "distinct_id", "ViewProduct", "event_id1", #{"key1" => "value1", "key2" => "value2"}),
  %% overwrite event
  thinking_analytics_sdk:track_overwrite_instance(TE_SDK1, AccountId1, "distinct_id", "ViewProduct", "event_id", #{"key1" => "value1", "key2" => "value2"}),

  %% user properties
  Name = <<228,184,173,230,150,135,95,97,95,49,50,51>>,
  thinking_analytics_sdk:user_set_instance(TE_SDK, AccountId0, "distinct_id", #{"id" => 12, "key_1" => [1,1,1,1], "key_2" => ["a", "b"], "key_3" => ["中", "文"], "key_4" => ["中文", "list"], "key_5" => "中文字符串", "amount" => 7.123, "nickName" => Name}),
  thinking_analytics_sdk:user_add_instance(TE_SDK, AccountId0, "distinct_id", #{"amount" => 100}),
  thinking_analytics_sdk:user_unset_instance(TE_SDK, AccountId0, "distinct_id", ["age", "abc"]),
  thinking_analytics_sdk:user_set_once_instance(TE_SDK1, AccountId1, "distinct_id", #{"firstValue" => 1}),
  thinking_analytics_sdk:user_append_instance(TE_SDK1, AccountId1, "distinct_id", #{"array" => ["arr3", "arr4"]}),
  thinking_analytics_sdk:user_unique_append_instance(TE_SDK1, AccountId1, "distinct_id", #{"array" => ["arr3", "arr4", "arr5"]}),
  thinking_analytics_sdk:user_del_instance(TE_SDK1, AccountId1, "distinct_id"),

  thinking_analytics_sdk:flush_instance(TE_SDK),
  thinking_analytics_sdk:flush_instance(TE_SDK1),

  ok.


