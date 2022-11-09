%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5月 2022 10:40
%%%-------------------------------------------------------------------
-module(ta_consumer_debug).
-author("ThinkingData").

%% API
-export([add/1,
  flush/0,
  close/0,
  config_server_url/1,
  config_app_id/1,
  config_is_write/1,
  init/0,
  config_device_id/1
]).

%% debugConsumer使用的ets表名
-define(TABLE, ta_table_debug_consumer).

-define(SERVER_URL, server_url).
-define(APP_ID, app_id).
-define(DEVICE_ID, device_id).
-define(IS_WRITE, is_write).

%% 配置url
-spec config_server_url(string()) -> _.
config_server_url(Url) ->
  set_value2ets(?SERVER_URL, Url).

get_server_url() ->
  Value = find_value_from_ets(?SERVER_URL),
  if
    length(Value) > 0 -> Value;
    true -> throw("thinking data error: server url must not be nil!")
  end.

%% 配置app_id
-spec config_app_id(string()) -> _.
config_app_id(AppId) ->
  set_value2ets(?APP_ID, AppId).

get_app_id() ->
  Value = find_value_from_ets(?APP_ID),
  if
    length(Value) > 0 -> Value;
    true -> throw("thinking data error: app id url must not be nil!")
  end.

%% 配置是否写入到数据库
-spec config_is_write(boolean()) -> _.
config_is_write(IsWrite) ->
  set_value2ets(?IS_WRITE, IsWrite).

get_is_write() ->
  Value = find_value_from_ets(?IS_WRITE),
  if
    is_boolean(Value) -> Value;
    true -> true
  end.

%% 配置 DeviceId
-spec config_device_id(string()) -> _.
config_device_id(DeviceId) ->
  set_value2ets(?DEVICE_ID, DeviceId).

get_device_id() ->
  Value = find_value_from_ets(?DEVICE_ID),
  if
    length(Value) > 0 -> Value;
    true -> ""
  end.

-spec init() -> _.
init() ->
  %% 创建ETS表
  ets:new(?TABLE, [set, named_table, public]).

-spec add(thinking_analytics_sdk:event()) -> _.
add(E) ->
  inets:start(),
  DryRun = case get_is_write() of
             true -> "0";
             false -> "1"
           end,
  AppId = get_app_id(),

  RequestBody = lists:concat(["data=", E, "&appid=", AppId, "&source=", "server", "&dryRun=", DryRun]),

  DeviceId = get_device_id(),
  RequestBody1 = if
    length(DeviceId) > 0 -> lists:concat([RequestBody, "&deviceId=", DeviceId]);
    true -> RequestBody
  end,

  ContentType = "application/x-www-form-urlencoded",
  HTTPOptions = [{timeout, 30000}],
  Options = [],
  Url = string:concat(get_server_url(), "/data_debug"),
  Result = try
           {ok, {{_Version, 200 , _ReasonPhrase}, _Headers, Body}} =
             httpc:request(post, {Url, [], ContentType, RequestBody1}, HTTPOptions, Options),
           Body
         catch
           error:Error -> lager:error("~p", [Error])
         end,

  lager:info("result: ~s~n", [Result]).

%% 立即发送。暂时不需要调用
-spec flush() -> _.
flush() ->
  [].

%% 关闭SDK
-spec close() -> _.
close() ->
  %% 删除ETS表
  ets:delete(?TABLE).

%% 在 ets 中根据key设置value
-spec set_value2ets(string(), string()) -> _.
set_value2ets(Key, Value) ->
  try
    ets:insert(?TABLE, {Key, Value})
  catch
    error:_ -> lager:error("thinking data error: set value to ets failed key:~p .~n", [Key])
  end.

%% 在 ets 中根据key查找value
-spec find_value_from_ets(string()) -> _.
find_value_from_ets(Key) ->
  try
    [{_, E}|_] = ets:lookup(?TABLE, Key),
    E
  catch
    error:_ -> []
  end.