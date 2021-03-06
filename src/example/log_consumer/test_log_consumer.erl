%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5月 2022 10:27
%%%-------------------------------------------------------------------
-module(test_log_consumer).
-author("ThinkingData").

%% API
-export([test/0]).

test() ->
  %% 必须先调用init，SDK内部进行必要的初始化
  ta_consumer_log:init(),
  %% 配置写入文件的路径
  ta_consumer_log:config_directory("/Users/Shared/log"),
  %% 配置写入文件的前缀
  ta_consumer_log:config_file_name_prefix("ta"),
  %% 配置日志文件的最大切片大小，单位Mb
  ta_consumer_log:config_file_size(2),
  %% 配置日志文件的划分格式，以天为单位，或者以小时为单位
  ta_consumer_log:config_rotate_mode(ta_consumer_log:rotate_mode_hour()),

  %% 配置完成之后，开始初始化 thinking_analytics_sdk 模块
  %% 初始化SDK，传入上报方式类型
  thinking_analytics_sdk:init(thinking_analytics_sdk:consumer_type_log()),

  %% 普通事件
  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"key_1" => "value_1", "key_2" => "value_2"}),
%%  %% 复杂数据类型
%%  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"child_key" => "child_value"}]}]}),
%%
%%  %% ⚠️ 如果您的属性值有时间类型，请一定要用 ta_utils:format_time() 函数进行格式化，然后才能传入。例如：
%%  %% 包含时间的属性值
%%  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),
%%
%%  %% 首次事件
%%  thinking_analytics_sdk:track_first("account_id_Erlang", "distinct_id", "first_login", "1", #{"key1" => "value1", "key2" => "value2"}),
%%  %% 可更新事件
%%  thinking_analytics_sdk:track_update("account_id_Erlang", "distinct_id", "ViewProduct", "event_id1", #{"key1" => "value1", "key2" => "value2"}),
%%  %% 可重写事件
%%  thinking_analytics_sdk:track_overwrite("account_id_Erlang", "distinct_id", "ViewProduct", "event_id", #{"key1" => "value1", "key2" => "value2"}),
%%  %% 用户属性
%%  thinking_analytics_sdk:user_set("account_id_Erlang", "distinct_id", #{"age" => 18, "abc" => ["a", "b", "c"]}),
%%  thinking_analytics_sdk:user_add("account_id_Erlang", "distinct_id", #{"amount" => 100}),
%%  thinking_analytics_sdk:user_unset("account_id_Erlang", "distinct_id", ["age", "abc"]),
%%  thinking_analytics_sdk:user_set_once("account_id_Erlang", "distinct_id", #{"firstvalue" => 1}),
%%  thinking_analytics_sdk:user_append("account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4"]}),
%%  thinking_analytics_sdk:user_unique_append("account_id_Erlang", "distinct_id", #{"array" => ["arr3", "arr4", "arr5"]}),
%%  thinking_analytics_sdk:user_del("account_id_Erlang", "distinct_id"),

  %% 关闭SDK时候需要调用
  thinking_analytics_sdk:close().




