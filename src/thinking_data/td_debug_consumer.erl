%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Used to verify data accuracy
%%% @end
%%% Created : 16. 5Month 2022 10:40
%%%-------------------------------------------------------------------
-module(td_debug_consumer).
-author("ThinkingData").
-include("td_analytics.hrl").

%% API
-export([add/1,
  flush/0,
  close/0,
  config_server_url/1,
  config_app_id/1,
  config_is_write/1,
  init/0,
  config_device_id/1,
  init_with_config/4,
  flush_with_instance/1,
  close_with_instance/1,
  add_with_instance/2
]).

%% ETS table names which debugConsumer used.
-define(TABLE, ta_table_debug_consumer).

-define(SERVER_URL, server_url).
-define(APP_ID, app_id).
-define(DEVICE_ID, device_id).
-define(IS_WRITE, is_write).

-type server_url() :: string().
-type app_id() :: string().
-type is_write() :: boolean().
-type device_id() :: string().

%% @doc Set server url
-spec config_server_url(string()) -> _.
config_server_url(Url) ->
  set_value2ets(?SERVER_URL, Url).

%% @doc Get server url
get_server_url() ->
  Value = find_value_from_ets(?SERVER_URL),
  if
    length(Value) > 0 -> Value;
    true -> throw("thinking data error: server url must not be nil!")
  end.

%% @doc Set app id
-spec config_app_id(string()) -> _.
config_app_id(AppId) ->
  set_value2ets(?APP_ID, AppId).

%% @doc Get app id
get_app_id() ->
  Value = find_value_from_ets(?APP_ID),
  if
    length(Value) > 0 -> Value;
    true -> throw("thinking data error: app id url must not be nil!")
  end.

%% @doc Whether to record the debug data to TE. True: record the data. False: don't record the data, validate only
-spec config_is_write(boolean()) -> _.
config_is_write(IsWrite) ->
  set_value2ets(?IS_WRITE, IsWrite).

%% @doc Get status of whether to record debug data
get_is_write() ->
  Value = find_value_from_ets(?IS_WRITE),
  if
    is_boolean(Value) -> Value;
    true -> true
  end.

%% @doc Set device id
-spec config_device_id(string()) -> _.
config_device_id(DeviceId) ->
  set_value2ets(?DEVICE_ID, DeviceId).

%% @doc Get device id
get_device_id() ->
  Value = find_value_from_ets(?DEVICE_ID),
  if
    length(Value) > 0 -> Value;
    true -> ""
  end.

%% @doc Init debug consumer
-spec init() -> _.
init() ->
  inets:start(),
  %% create ETS table.
  ets:new(?TABLE, [set, named_table, public]).

%% @doc Init debug consumer with config
-spec init_with_config(server_url(), app_id(), is_write(), device_id()) -> _.
init_with_config(ServerUrl, AppId, IsWrite, DeviceId) ->
  inets:start(),
  #td_debug_consumer{server_url = ServerUrl, app_id = AppId, is_write = IsWrite, device_id = DeviceId}.

%% @doc Add event
-spec add(thinking_analytics_sdk:event()) -> _.
add(E) ->
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
  lager:info("result: ~ts~n", [unicode:characters_to_list(list_to_binary(Result))]).

%% @doc (API_v2) Add event
-spec add_with_instance(td_analytics:td_debug_consumer(), td_analytics:event()) -> _.
add_with_instance(Consumer, E) ->
  DryRun = case Consumer#td_debug_consumer.is_write of
             true -> "0";
             false -> "1"
           end,
  AppId = Consumer#td_debug_consumer.app_id,

  RequestBody = lists:concat(["data=", E, "&appid=", AppId, "&source=", "server", "&dryRun=", DryRun]),

  DeviceId = Consumer#td_debug_consumer.device_id,
  RequestBody1 = if
                   length(DeviceId) > 0 -> lists:concat([RequestBody, "&deviceId=", DeviceId]);
                   true -> RequestBody
                 end,

  ContentType = "application/x-www-form-urlencoded",
  HTTPOptions = [{timeout, 30000}],
  Options = [],
  Url = string:concat(Consumer#td_debug_consumer.server_url, "/data_debug"),
  Result = try
             {ok, {{_Version, 200 , _ReasonPhrase}, _Headers, Body}} =
               httpc:request(post, {Url, [], ContentType, RequestBody1}, HTTPOptions, Options),
             Body
           catch
             error:Error -> lager:error("~p", [Error])
           end,
  lager:info("result: ~ts~n", [unicode:characters_to_list(list_to_binary(Result))]).

%% @doc Flush data immediately. don't need invoke.
-spec flush() -> _.
flush() ->
  [].

%% @doc Close SDK
-spec close() -> _.
close() ->
  %% delete ETS table
  ets:delete(?TABLE).

%% @doc (API_v2) Flush data immediately. don't need invoke.
-spec flush_with_instance(td_analytics:td_debug_consumer()) -> _.
flush_with_instance(Consumer) ->
  Consumer,
  ok.

%% @doc (API_v2) Close SDK
-spec close_with_instance(td_analytics:td_debug_consumer()) -> _.
close_with_instance(Consumer) ->
  Consumer,
  ok.

%% set value for key to ETS
-spec set_value2ets(string(), string()) -> _.
set_value2ets(Key, Value) ->
  try
    ets:insert(?TABLE, {Key, Value})
  catch
    error:_ -> lager:error("thinking data error: set value to ets failed key:~p .~n", [Key])
  end.

%% Find value whith key in ETS
-spec find_value_from_ets(string()) -> _.
find_value_from_ets(Key) ->
  try
    [{_, E}|_] = ets:lookup(?TABLE, Key),
    E
  catch
    error:_ -> []
  end.