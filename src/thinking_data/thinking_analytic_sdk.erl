%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5月 2022 10:39
%%%-------------------------------------------------------------------
-module(thinking_analytic_sdk).
-author("ThinkingData").

%% API
-export([init/1
  , track/4
  , track_first/5
  , user_unset/3
  , user_set/3
  , track_update/5
  , track_overwrite/5
  , user_set_once/3
  , user_del/2
  , user_unique_append/3
  , user_append/3
  , user_add/3
  , close/0
  , flush/0
  , consumer_type_log/0
  , consumer_type_debug/0]).

-export_type([operation/0, event/0, consumer_type/0]).

-define(TA_TABLE, ta_table).
-define(TA_OPERATION, "operation").

-define(TRACK, "track").
-define(TRACK_UPDATE, "track_update").
-define(TRACK_OVERWRITE, "track_overwrite").
-define(USER_SET, "user_set").
-define(USER_UNSET, "user_unset").
-define(USER_SET_ONCE, "user_setOnce").
-define(USER_ADD, "user_add").
-define(USER_APPEND, "user_append").
-define(USER_UNIQUE_APPEND, "user_uniq_append").
-define(USER_DEL, "user_del").

-define(FIRST_CHECK_ID, "#first_check_id").

-define(SDK_VERSION, "1.0.0").
-define(LIB_NAME, "Erlang").

%% 函数名
-define(FUNC_ADD, add).
-define(FUNC_FLUSH, flush).
-define(FUNC_CLOSE, close).

%% consumer 类型
-define(CONSUMER_TYPE_LOG, consumer_log).
%% consumer 类型
-define( CONSUMER_TYPE_DEBUG, consumer_debug).

-type consumer_type() :: ?CONSUMER_TYPE_LOG | ? CONSUMER_TYPE_DEBUG.

-type operation() :: #{ ?FUNC_ADD => fun()
                      , ?FUNC_FLUSH => fun()
                      , ?FUNC_CLOSE => fun()
                      }.

-type account_id() :: string().
-type distinct_id() :: string().
-type event_name() :: string().
-type first_check_id() :: string().
-type properties() :: map().
-type event_type() :: string().
-type event_id() :: string().

-type event() :: #{ eventName => event_name()
                  , accountId => account_id()
                  , distinctId => distinct_id()
                  , type => event_type()
                  , time => string()
                  , eventId => event_id()
                  , firstCheckId => first_check_id()
                  , ip => string()
                  , uuid => string()
                  , properties => properties()
                  }.

-spec consumer_type_log() -> _.
consumer_type_log() ->
  ?CONSUMER_TYPE_LOG.

-spec consumer_type_debug() -> _.
consumer_type_debug() ->
  ?CONSUMER_TYPE_DEBUG.

-spec init(consumer_type()) -> _.
init(Type) ->
  %% 创建ETS表
  ets:new(?TA_TABLE, [set, named_table, public]),
  %% 创建operation 数据，根据类型选择add/flush/close方法
  Operation = case Type of
                ?CONSUMER_TYPE_LOG -> #{
                  ?FUNC_ADD => fun ta_consumer_log:add/1
                  , ?FUNC_FLUSH => fun ta_consumer_log:flush/0
                  , ?FUNC_CLOSE => fun ta_consumer_log:close/0
                };
                ?CONSUMER_TYPE_DEBUG -> #{
                  ?FUNC_ADD => fun ta_consumer_debug:add/1
                  , ?FUNC_FLUSH => fun ta_consumer_debug:flush/0
                  , ?FUNC_CLOSE => fun ta_consumer_debug:close/0
                };
                _ -> #{}
              end,
  ets:insert(?TA_TABLE, {?TA_OPERATION, Operation}).

%% 追踪一个普通事件
-spec track(account_id(), distinct_id(), event_name(), properties()) -> _.
track(AccountId, DistinctId, EventName, Properties) ->
  internal_track(AccountId, DistinctId, ?TRACK, EventName, "", Properties).

%% 首次事件
-spec track_first(account_id(), distinct_id(), event_name(), first_check_id(), properties()) -> _.
track_first(AccountId, DistinctId, EventName, FirstCheckId, Properties) ->
  if
    length(FirstCheckId) > 0 ->
      Params = Properties#{?FIRST_CHECK_ID => FirstCheckId},
      internal_track(AccountId, DistinctId, ?TRACK, EventName, "", Params);
    true -> throw("thinking data error: first_check_id not be nil.~n")
  end.

%% 可更新事件
-spec track_update(account_id(), distinct_id(), event_name(), event_id(), properties()) -> _.
track_update(AccountId, DistinctId, EventName, EventId, Properties) ->
  internal_track(AccountId, DistinctId, ?TRACK_UPDATE, EventName, EventId, Properties).

%% 可重写事件
-spec track_overwrite(account_id(), distinct_id(), event_name(), event_id(), properties()) -> _.
track_overwrite(AccountId, DistinctId, EventName, EventId, Properties) ->
  internal_track(AccountId, DistinctId, ?TRACK_OVERWRITE, EventName, EventId, Properties).

%% 所有track事件的处理函数
-spec internal_track(account_id(), distinct_id(), event_type(), event_name(), event_id(), properties()) -> _.
internal_track(AccountId, DistinctId, EventType, EventName, EventId, Properties) ->
  %% EventName 不能为空
  if
    length(EventName) =< 0 -> throw("thinking data error: the event name must be provided");
    true -> []
  end,

  %% 只有eventType == Track 的时候才不需要eventId，否则eventId不能为空
  if
    EventType /= ?TRACK, length(EventId) =< 0 -> throw("thinking data error: the EventId must be provided");
    true -> []
  end,

  %% 获取设置的公共属性
  %% 获取设置的动态公共属性

  %% 设置预制属性
  NewProperties = Properties#{"#lib" => ?LIB_NAME, "#lib_version" => ?SDK_VERSION},

  %% 构造事件
  generate_event(AccountId, DistinctId, EventType, EventName, EventId, NewProperties).

%% 设置用户属性. 如果同名属性已存在，则用传入的属性覆盖同名属性
-spec user_set(account_id(), distinct_id(), properties()) -> _.
user_set(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_SET, Properties).

%% 删除用户属性
-spec user_unset(account_id(), distinct_id(), erlang:list()) -> _.
user_unset(AccountId, DistinctId, PropertiesList) ->
  NewList = lists:map(fun(E) -> {E, 0} end, PropertiesList),
  Properties = maps:from_list(NewList),
  internal_user(AccountId, DistinctId, ?USER_UNSET, Properties).

%% 设置用户属性. 不会覆盖同名属性.
-spec user_set_once(account_id(), distinct_id(), properties()) -> _.
user_set_once(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_SET_ONCE, Properties).

%% 对数值类型的属性做累加操作
-spec user_add(account_id(), distinct_id(), properties()) -> _.
user_add(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_ADD, Properties).

%% 对数组类型的属性做追加加操作
-spec user_append(account_id(), distinct_id(), properties()) -> _.
user_append(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_APPEND, Properties).

%% 对数组类型的属性做追加加操作，对于重复元素进行去重处理
-spec user_unique_append(account_id(), distinct_id(), properties()) -> _.
user_unique_append(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_UNIQUE_APPEND, Properties).

%% 删除用户数据, 之后无法查看用户属性, 但是之前已经入库的事件数据不会被删除. 此操作不可逆
-spec user_del(account_id(), distinct_id()) -> _.
user_del(AccountId, DistinctId) ->
  internal_user(AccountId, DistinctId, ?USER_DEL, #{}).

%% 所有用户属性的操作函数
-spec internal_user(account_id(), distinct_id(), event_type(), properties()) -> _.
internal_user(AccountId, DistinctId, EventType, Properties) ->
  %% 除了删除操作，否则属性不能为空
  if
    EventType /= ?USER_DEL, map_size(Properties) == 0 -> throw("thinking data error: Properties not be nil");
    true -> []
  end,
  %% 构造事件
  generate_event(AccountId, DistinctId, EventType, "", "", Properties).

%% 构造最终的event数据
-spec generate_event(account_id(), distinct_id(), event_type(), event_name(), event_id(), properties()) -> _.
generate_event(AccountId, DistinctId, EventType, EventName, EventId, Properties) ->
  %% accountId 和 distinctId 不能同时为空
  if
    length(AccountId) == 0, length(DistinctId) == 0 -> throw("thinking data error: account_id and distinct_id cannot be empty at the same time");
    true -> []
  end,

  %% 从 properties 中过滤系统属性

  %% 获取 properties 中 #ip 值, 如不存在则返回 ""
  {IP, Map1} = filter_system_properties("#ip", Properties),

  %% 获取 properties 中 #time 值, 如不存在则返回当前时间
  {ValueTime, Map2} = filter_time(Map1),
  %% 格式化时间
  Time = format_time(ValueTime),

  %% 获取 properties 中 #first_check_id 值, 如不存在则返回 ""
  {FirstCheckId, Map3} = filter_system_properties(?FIRST_CHECK_ID, Map2),

  %% 如果上传#uuid， 只支持UUID标准格式xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx的string类型
  {UUID, Map4} = filter_system_properties("#uuid", Map3),

  NewUUID = if
              length(UUID) == 0 -> generate_uuid();
              true -> UUID
            end,

  Event = #{
    "#account_id" => AccountId,
    "#distinct_id" => DistinctId,
    "#type" => EventType,
    "#time" => Time,
    "#event_name" => EventName,
    "#event_id" => EventId,
    "#first_check_id" => FirstCheckId,
    "#ip" => IP,
    "#uuid" => NewUUID,
    "properties" => Map4
  },

  %% 过滤空的value
  Event1 = maps:fold(fun(Key, Value, AccIn) ->
    NewValue = if
                 is_list(Value) ->
                   if
                     length(Value) > 0 -> true;
                     true -> false
                   end;
                   true -> true
               end,
    case NewValue of
      true -> AccIn#{Key => Value};
      false -> AccIn
    end
                     end, #{}, Event),

  %% value 如果是string就转化成binary类型
  NewEvent = convert_string2binary(Event1),
  %% json 字符串
  JsonEvent = binary_to_list(ta_thoas:encode(NewEvent)),
  %% 调用consumer的add函数
  Add = find_function(?FUNC_ADD),
  if
    is_function(Add) -> Add(JsonEvent);
    true -> []
  end.

%% 将Maps结构中value为string的类型，转化为binary类型。
-spec convert_string2binary(#{}) -> #{}.
convert_string2binary(Map) ->
  maps:fold(fun(Key, Value, AccIn) ->
    NewValue = if
                 is_list(Value) ->
                   %% 是否是字符串，区别list 与 字符串
                   case io_lib:printable_list(Value) of
                     false -> lists:map(fun erlang:list_to_binary/1, Value);
                     true -> list_to_binary(Value)
                   end;
                 is_map(Value) -> convert_string2binary(Value);
                 true -> Value
               end,
    AccIn#{Key => NewValue}
            end, #{}, Map).

%% 从属性字典中找到系统属性的值，并删除
-spec filter_system_properties(string(), #{}) -> {string(), #{}}.
filter_system_properties(Key, Map) ->
  try
    {_, Value} = maps:find(Key, Map),
    NewMap = maps:remove(Key, Map),
    {Value, NewMap}
  catch
    error:_ -> {[], Map}
  end.

-spec filter_time(#{}) -> {erlang:timestamp(), #{}}.
filter_time(Map) ->
  try
    {_, Value} = maps:find("#time", Map),
    NewMap = maps:remove("#time", Map),
    {Value, NewMap}
  catch
    error:_ -> {os:timestamp(), Map}
  end.

%% 格式化时间
-spec format_time(erlang:timestamp()) -> string().
format_time(Time) ->
  {M, S, Micro} = Time,
  DT = calendar:gregorian_seconds_to_datetime(M * 1000000 + S +
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})),
  {{Y, Month, D}, {H, Minute, Second}} = calendar:universal_time_to_local_time(DT),
  FormatMonth = format_integer(Month, 2, ""),
  FormatDay = format_integer(D, 2, ""),
  FormatHour = format_integer(H, 2, ""),
  FormatMinute = format_integer(Minute, 2, ""),
  FormatSecond = format_integer(Second, 2, ""),
  FormatMill = format_integer((Micro div 1000), 3, ""),
  lists:concat([Y, "-", FormatMonth, "-", FormatDay, " ", FormatHour, ":", FormatMinute, ":", FormatSecond, ".", FormatMill]).

%% 输入Num数字，转化为Places位小数, Init为初始化字符串内容，一般为空字符串""
-spec format_integer(integer(), integer(), string()) -> string().
format_integer(_Num, 0, Init) ->
  Init;
format_integer(Num, Places, Init) ->
  %% 进制位，当Places == 3, 那么就是10的2次方，表示100。
  Format = trunc(math:pow(10, Places - 1)),
  %% 权重
  Weight = Num div Format,
  %% 剩下的数字
  Unit = Num rem Format,
  format_integer(Unit, Places - 1, string:concat(Init, integer_to_list(Weight))).

%% 创建一个uuid
-spec generate_uuid() -> string().
generate_uuid() ->
  ta_uuid:v4_string().

%% 立即上报数据
-spec flush() -> _.
flush() ->
  Flush = find_function(?FUNC_FLUSH),
  if
    is_function(Flush) -> Flush();
    true -> []
  end.

%% 关闭SDK
-spec close() -> _.
close() ->
  Close = find_function(?FUNC_CLOSE),
  if
    is_function(Close) -> Close();
    true -> []
  end,

  %% 删除ETS表
  ets:delete(?TA_TABLE).

%% 在 ets 中查找策略对象，找到目标函数
-spec find_function(string()) -> fun().
find_function(Name) ->
  try
    [{_, E}|_] = ets:lookup(?TA_TABLE, ?TA_OPERATION),
    {_, Func} = maps:find(Name, E),
    Func
  catch
    error:_ -> io:format("thinking data error: not match function.~n"), []
  end.
