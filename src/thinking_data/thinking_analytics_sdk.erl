%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5Month 2022 10:39
%%%-------------------------------------------------------------------
-module(thinking_analytics_sdk).
-author("ThinkingData").

-include("thinking_engine_sdk.hrl").

%% API
-export([
  consumer_type_log/0,
  consumer_type_debug/0,
  init/1,
  track/4,
  track_first/5,
  track_update/5,
  track_overwrite/5,
  user_set/3,
  user_unset/3,
  user_set_once/3,
  user_append/3,
  user_unique_append/3,
  user_add/3,
  user_del/2,
  close/0,
  flush/0,
  init_with_consumer/1,
  track_instance/5,
  track_first_instance/6,
  track_update_instance/6,
  track_overwrite_instance/6,
  user_set_instance/4,
  user_unset_instance/4,
  user_set_once_instance/4,
  user_append_instance/4,
  user_unique_append_instance/4,
  user_add_instance/4,
  user_del_instance/3,
  flush_instance/1,
  close_instance/1
]).

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

-define(SDK_VERSION, "1.3.0").
-define(LIB_NAME, "Erlang").

%% common function name
-define(FUNC_ADD, add).
-define(FUNC_FLUSH, flush).
-define(FUNC_CLOSE, close).

%% log consumer
-define(CONSUMER_TYPE_LOG, consumer_log).
%% debug consumer
-define( CONSUMER_TYPE_DEBUG, consumer_debug).

-type consumer_type() :: ?CONSUMER_TYPE_LOG | ? CONSUMER_TYPE_DEBUG.

-type operation() :: #{ ?FUNC_ADD => fun()
                      , ?FUNC_FLUSH => fun()
                      , ?FUNC_CLOSE => fun()
                      }.

-type te_sdk() :: #te_sdk{}.
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
  %% create ETS table
  ets:new(?TA_TABLE, [set, named_table, public]),
  %% init Operation. it store specific function whith different Consumer
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

%% ordinary event
-spec track(account_id(), distinct_id(), event_name(), properties()) -> _.
track(AccountId, DistinctId, EventName, Properties) ->
  internal_track(AccountId, DistinctId, ?TRACK, EventName, "", Properties).

%% first event
-spec track_first(account_id(), distinct_id(), event_name(), first_check_id(), properties()) -> _.
track_first(AccountId, DistinctId, EventName, FirstCheckId, Properties) ->
  NewFirstCheckId = format2string(FirstCheckId),
  if
    length(NewFirstCheckId) > 0 ->
      Params = Properties#{?FIRST_CHECK_ID => FirstCheckId},
      internal_track(AccountId, DistinctId, ?TRACK, EventName, "", Params);
    true -> throw("thinking data error: first_check_id not be nil.~n")
  end.

%% updatable event
-spec track_update(account_id(), distinct_id(), event_name(), event_id(), properties()) -> _.
track_update(AccountId, DistinctId, EventName, EventId, Properties) ->
  internal_track(AccountId, DistinctId, ?TRACK_UPDATE, EventName, EventId, Properties).

%% overwritable event
-spec track_overwrite(account_id(), distinct_id(), event_name(), event_id(), properties()) -> _.
track_overwrite(AccountId, DistinctId, EventName, EventId, Properties) ->
  internal_track(AccountId, DistinctId, ?TRACK_OVERWRITE, EventName, EventId, Properties).

%% set user properties. would overwrite existing names
-spec user_set(account_id(), distinct_id(), properties()) -> _.
user_set(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_SET, Properties).

%% clear the user properties of users
-spec user_unset(account_id(), distinct_id(), erlang:list()) -> _.
user_unset(AccountId, DistinctId, PropertiesList) ->
  NewList = lists:map(fun(E) -> {E, 0} end, PropertiesList),
  Properties = maps:from_list(NewList),
  internal_user(AccountId, DistinctId, ?USER_UNSET, Properties).

%% If such property had been set before, this message would be neglected.
-spec user_set_once(account_id(), distinct_id(), properties()) -> _.
user_set_once(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_SET_ONCE, Properties).

%% to accumulate operations against the property
-spec user_add(account_id(), distinct_id(), properties()) -> _.
user_add(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_ADD, Properties).

%% to add user properties of array type
-spec user_append(account_id(), distinct_id(), properties()) -> _.
user_append(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_APPEND, Properties).

%% add user properties of array type. delete duplicated user property
-spec user_unique_append(account_id(), distinct_id(), properties()) -> _.
user_unique_append(AccountId, DistinctId, Properties) ->
  internal_user(AccountId, DistinctId, ?USER_UNIQUE_APPEND, Properties).

%% delete a user
-spec user_del(account_id(), distinct_id()) -> _.
user_del(AccountId, DistinctId) ->
  internal_user(AccountId, DistinctId, ?USER_DEL, #{}).

-spec flush() -> _.
flush() ->
  Flush = find_function(?FUNC_FLUSH),
  if
    is_function(Flush) -> Flush();
    true -> []
  end.

-spec close() -> _.
close() ->
  Close = find_function(?FUNC_CLOSE),
  if
    is_function(Close) -> Close();
    true -> []
  end,
  ets:delete(?TA_TABLE).

%% V2 API. support multiple instance.

-spec init_with_consumer(#{}) -> _.
init_with_consumer(Consumer) ->
  if
    is_record(Consumer, te_log_consumer) ->
      io:format("TE init log consumer. ~n"),
      #te_sdk{consumer = Consumer, fun_add = fun ta_consumer_log:add_with_instance/2, fun_flush = fun ta_consumer_log:flush_with_instance/1, fun_close = fun ta_consumer_log:close_with_instance/1};
    true ->
      if
        is_record(Consumer, te_debug_consumer) ->
          io:format("TE init debug consumer.~n"),
          #te_sdk{consumer = Consumer, fun_add = fun ta_consumer_debug:add_with_instance/2, fun_flush = fun ta_consumer_debug:flush_with_instance/1, fun_close = fun ta_consumer_debug:close_with_instance/1};
        true -> throw("TE Consumer error. ~n")
      end
  end.

%% ordinary event
-spec track_instance(te_sdk(), account_id(), distinct_id(), event_name(), properties()) -> _.
track_instance(Instance, AccountId, DistinctId, EventName, Properties) ->
  internal_track_instance(Instance, AccountId, DistinctId, ?TRACK, EventName, "", Properties).

%% first event
-spec track_first_instance(te_sdk(), account_id(), distinct_id(), event_name(), first_check_id(), properties()) -> _.
track_first_instance(Instance, AccountId, DistinctId, EventName, FirstCheckId, Properties) ->
  NewFirstCheckId = format2string(FirstCheckId),
  if
    length(NewFirstCheckId) > 0 ->
      Params = Properties#{?FIRST_CHECK_ID => FirstCheckId},
      internal_track_instance(Instance, AccountId, DistinctId, ?TRACK, EventName, "", Params);
    true -> throw("thinking data error: first_check_id not be nil.~n")
  end.

%% updatable event
-spec track_update_instance(te_sdk(), account_id(), distinct_id(), event_name(), event_id(), properties()) -> _.
track_update_instance(Instance, AccountId, DistinctId, EventName, EventId, Properties) ->
  internal_track_instance(Instance, AccountId, DistinctId, ?TRACK_UPDATE, EventName, EventId, Properties).

%% overwrite event
-spec track_overwrite_instance(te_sdk(), account_id(), distinct_id(), event_name(), event_id(), properties()) -> _.
track_overwrite_instance(Instance, AccountId, DistinctId, EventName, EventId, Properties) ->
  internal_track_instance(Instance, AccountId, DistinctId, ?TRACK_OVERWRITE, EventName, EventId, Properties).

%% set user properties. would overwrite existing names
-spec user_set_instance(te_sdk(), account_id(), distinct_id(), properties()) -> _.
user_set_instance(Instance, AccountId, DistinctId, Properties) ->
  internal_user_instance(Instance, AccountId, DistinctId, ?USER_SET, Properties).

%% clear the user properties of users
-spec user_unset_instance(te_sdk(), account_id(), distinct_id(), erlang:list()) -> _.
user_unset_instance(Instance, AccountId, DistinctId, PropertiesList) ->
  NewList = lists:map(fun(E) -> {E, 0} end, PropertiesList),
  Properties = maps:from_list(NewList),
  internal_user_instance(Instance, AccountId, DistinctId, ?USER_UNSET, Properties).

%% If such property had been set before, this message would be neglected.
-spec user_set_once_instance(te_sdk(), account_id(), distinct_id(), properties()) -> _.
user_set_once_instance(Instance, AccountId, DistinctId, Properties) ->
  internal_user_instance(Instance, AccountId, DistinctId, ?USER_SET_ONCE, Properties).

%% to accumulate operations against the property
-spec user_add_instance(te_sdk(), account_id(), distinct_id(), properties()) -> _.
user_add_instance(Instance, AccountId, DistinctId, Properties) ->
  internal_user_instance(Instance, AccountId, DistinctId, ?USER_ADD, Properties).

%% to add user properties of array type
-spec user_append_instance(te_sdk(), account_id(), distinct_id(), properties()) -> _.
user_append_instance(Instance, AccountId, DistinctId, Properties) ->
  internal_user_instance(Instance, AccountId, DistinctId, ?USER_APPEND, Properties).

%% add user properties of array type. delete duplicated user property
-spec user_unique_append_instance(te_sdk(), account_id(), distinct_id(), properties()) -> _.
user_unique_append_instance(Instance, AccountId, DistinctId, Properties) ->
  internal_user_instance(Instance, AccountId, DistinctId, ?USER_UNIQUE_APPEND, Properties).

%% delete a user
-spec user_del_instance(te_sdk(), account_id(), distinct_id()) -> _.
user_del_instance(Instance, AccountId, DistinctId) ->
  internal_user_instance(Instance, AccountId, DistinctId, ?USER_DEL, #{}).

-spec flush_instance(#te_sdk{}) -> _.
flush_instance(SDK) ->
  Flush = SDK#te_sdk.fun_flush,
  Flush(SDK#te_sdk.consumer).

-spec close_instance(#te_sdk{}) -> _.
close_instance(SDK) ->
  Close = SDK#te_sdk.fun_close,
  Close(SDK#te_sdk.consumer).

%% Private methods

%% process event
-spec internal_track(account_id(), distinct_id(), event_type(), event_name(), event_id(), properties()) -> _.
internal_track(AccountId, DistinctId, EventType, EventName, EventId, Properties) ->
  validate_track_event(AccountId, DistinctId, EventType, EventName, EventId),
  %% set preset properties
  NewProperties = add_preset_properties(Properties),
  %% generate event
  JsonEvent = generate_event(AccountId, DistinctId, EventType, EventName, EventId, NewProperties),
  %% invoke Add function
  Add = find_function(?FUNC_ADD),
  if
    is_function(Add) -> Add(JsonEvent);
    true -> []
  end.

%% process event
-spec internal_track_instance(te_sdk(), account_id(), distinct_id(), event_type(), event_name(), event_id(), properties()) -> _.
internal_track_instance(Instance, AccountId, DistinctId, EventType, EventName, EventId, Properties) ->
  validate_track_event(AccountId, DistinctId, EventType, EventName, EventId),
  %% set preset properties
  NewProperties = add_preset_properties(Properties),
  %% generate event
  JsonEvent = generate_event(AccountId, DistinctId, EventType, EventName, EventId, NewProperties),
  %% invoke Add function
  Add = Instance#te_sdk.fun_add,
  if
    is_function(Add) -> Add(Instance#te_sdk.consumer, JsonEvent);
    true -> []
  end.

%% process user properties
-spec internal_user(account_id(), distinct_id(), event_type(), properties()) -> _.
internal_user(AccountId, DistinctId, EventType, Properties) ->
  %% Properties don't be null unless EventType is equal USER_DEL
  if
    EventType /= ?USER_DEL, map_size(Properties) == 0 -> throw("thinking data error: Properties not be nil");
    true -> []
  end,
  JsonEvent = generate_event(AccountId, DistinctId, EventType, "", "", Properties),
  %% invoke Add function
  Add = find_function(?FUNC_ADD),
  if
    is_function(Add) -> Add(JsonEvent);
    true -> []
  end.

%% process user properties
-spec internal_user_instance(te_sdk(), account_id(), distinct_id(), event_type(), properties()) -> _.
internal_user_instance(Instance, AccountId, DistinctId, EventType, Properties) ->
  %% Properties don't be null unless EventType is equal USER_DEL
  if
    EventType /= ?USER_DEL, map_size(Properties) == 0 -> throw("thinking data error: Properties not be nil");
    true -> []
  end,
  JsonEvent = generate_event(AccountId, DistinctId, EventType, "", "", Properties),
  %% invoke Add function
  Add = Instance#te_sdk.fun_add,
  if
    is_function(Add) -> Add(Instance#te_sdk.consumer, JsonEvent);
    true -> []
  end.

%% generate event
-spec generate_event(account_id(), distinct_id(), event_type(), event_name(), event_id(), properties()) -> _.
generate_event(AccountId, DistinctId, EventType, EventName, EventId, Properties) ->
  %% accountId and distinctId cannot be both empty
  if
    length(AccountId) == 0, length(DistinctId) == 0 -> throw("thinking data error: account_id and distinct_id cannot be empty at the same time");
    true -> []
  end,

  %% get "#ip" value in properties, empty string will be return when not found.
  {IP, Map1} = filter_system_properties("#ip", Properties),

  %% get "#time" value in properties, empty string will be return when not found.
  {ValueTime, Map2} = filter_time(Map1),
  %% format time
  Time = ta_utils:format_time(ValueTime),

  %% get "#first_check_id" value in properties, empty string will be return when not found.
  {FirstCheckId, Map3} = filter_system_properties(?FIRST_CHECK_ID, Map2),

  %% get "#uuid" value in properties, empty string will be return when not found.
  {UUID, Map4} = filter_system_properties("#uuid", Map3),

  NewUUID = if
              length(UUID) == 0 -> generate_uuid();
              true -> UUID
            end,

  %% get "#app_id" value in properties, empty string will be return when not found.
  {AppID, Map5} = filter_system_properties("#app_id", Map4),

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
    "#app_id" => AppID,
    "properties" => Map5
  },

  %% remove empty properties
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

  NewEvent = convert_string2binary(Event1),
  JsonEvent = binary_to_list(jsone:encode(NewEvent, [{float_format, [{decimals, 11}, compact]}])),
  JsonEvent.

%% convert string to binary
-spec convert_list2binary([]) -> binary().
convert_list2binary(Value) ->
  if
    is_list(Value) ->
      %% is normal string or not
      case io_lib:printable_list(Value) of
        false ->
          %% not normal string (include unicode, and list)
          case io_lib:printable_unicode_list(Value) of
            false ->
              %% normal list
              lists:map(fun(E) ->
                if
                  is_list(E) -> convert_list2binary(E);
                  is_map(E) -> convert_string2binary(E);
                  true -> E
                end
                        end, Value);
            true ->
              %% is unicode
              unicode:characters_to_binary(Value)
          end;
        true ->
          %% string which not include unicode
          list_to_binary(Value)
      end;
    is_map(Value) ->
      convert_string2binary(Value);
    true -> Value
  end.

%% convert string to binary
-spec convert_string2binary(#{}) -> #{}.
convert_string2binary(Map) ->
  maps:fold(fun(Key, Value, AccIn) ->
    NewValue = if
                 is_list(Value) -> convert_list2binary(Value);
                 is_map(Value) -> convert_string2binary(Value);
                 true ->
                   Value
               end,
    AccIn#{convert_list2binary(Key) => NewValue}
            end, #{}, Map).

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

-spec generate_uuid() -> string().
generate_uuid() ->
  ta_uuid:v4_string().

-spec format2string(string()|binary()) -> string().
format2string(Value) ->
  case is_binary(Value) of
    true -> binary_to_list(Value);
    false -> Value
  end.

-spec validate_track_event(account_id(), distinct_id(), event_type(), event_name(), event_id()) -> _.
validate_track_event(AccountId, DistinctId, EventType, EventName, EventId) ->
  NewAccountId = format2string(AccountId),
  NewDistinctId = format2string(DistinctId),
  NewEventName = format2string(EventName),
  NewEventId = format2string(EventId),

  %% accountId and distinctId cannot both be empty
  if
    (length(NewAccountId) =< 0) and (length(NewDistinctId) =< 0) -> throw("thinking data error: accountId and distinctId cannot both be empty");
    true -> ok
  end,

  %% EventName not be null
  if
    length(NewEventName) =< 0 -> throw("thinking data error: the event name must be provided");
    true -> ok
  end,

  %% eventId not be null unless eventType is equal Track.
  if
    EventType /= ?TRACK, length(NewEventId) =< 0 -> throw("thinking data error: the EventId must be provided");
    true -> ok
  end.

-spec add_preset_properties(properties()) -> properties().
add_preset_properties(Properties) ->
  Properties#{"#lib" => ?LIB_NAME, "#lib_version" => ?SDK_VERSION}.

%% ETS api

%% find truth function in strategy
-spec find_function(string()) -> fun().
find_function(Name) ->
  try
    [{_, E}|_] = ets:lookup(?TA_TABLE, ?TA_OPERATION),
    {_, Func} = maps:find(Name, E),
    Func
  catch
    error:_ -> lager:info("thinking data error: not match function.~n")
  end.