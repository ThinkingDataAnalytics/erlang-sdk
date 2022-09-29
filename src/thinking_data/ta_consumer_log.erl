%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5月 2022 10:40
%%%-------------------------------------------------------------------
-module(ta_consumer_log).
-author("ThinkingData").

-include_lib("kernel/include/file.hrl").

%% API
-export([
  add/1,
  flush/0,
  close/0,
  config_directory/1,
  config_rotate_mode/1,
  config_file_size/1,
  config_file_name_prefix/1,
  rotate_mode_hour/0,
  rotate_mode_daily/0,
  init/0]).

-export_type([rotate_mode/0]).

-type rotate_mode() :: rotate_daily | rotate_hour.

%% logConsumer使用的配置文件 ets 表名
-define(CONFIG_TABLE, ta_table_log_consumer).
%% 数据采集事件的缓存 ets 表名
-define(EVENT_TABLE, ta_table_event).

-define(PATH, path).
-define(ROTATE_MODE, rotate_mode).
-define(FILE_SIZE, file_size).
-define(FILE_NAME_PREFIX, file_name_prefix).

-spec rotate_mode_daily() -> rotate_mode().
rotate_mode_daily() ->
  rotate_mode_daily.

-spec rotate_mode_hour() -> rotate_mode().
rotate_mode_hour() ->
  rotate_hour.

-spec init() -> _.
init() ->
  %% 创建ETS表
  ets:new(?CONFIG_TABLE, [set, named_table, public]),
  ets:new(?EVENT_TABLE, [set, named_table, public]),
  %% 开启异步写入文件的循环
  Pid = spawn(fun() -> loop_write_to_file() end),
  register(tag_name_write, Pid).

%% 配置路径
-spec config_directory(string()) -> _.
config_directory(Path) ->
  set_value2ets(?PATH, Path).

get_directory() ->
  Value = find_value_from_ets(?PATH),
  if
    length(Value) > 0 -> Value;
    true -> "/ta_log"
  end.

%% 配置文件切割方式，按天，按小时
-spec config_rotate_mode(rotate_mode()) -> _.
config_rotate_mode(Mode) ->
  set_value2ets(?ROTATE_MODE, Mode).

get_rotate_mode() ->
  Value = find_value_from_ets(?ROTATE_MODE),
  if
    is_atom(Value) -> Value;
    true -> rotate_mode_daily()
  end.

%% 配置文件大小，单位 Megabyte
-spec config_file_size(string()) -> _.
config_file_size(Size) ->
  set_value2ets(?FILE_SIZE, Size).

get_file_size() ->
  Value = find_value_from_ets(?FILE_SIZE),
  if
    is_integer(Value) -> Value;
    true -> 0
  end.

%% 配置文件前缀
-spec config_file_name_prefix(string()) -> _.
config_file_name_prefix(Prefix) ->
  set_value2ets(?FILE_NAME_PREFIX, Prefix).

get_file_name_prefix() ->
  find_value_from_ets(?FILE_NAME_PREFIX).

-spec add(thinking_analytics_sdk:event()) -> _.
add(E) ->
  add_event2ets(E).

-spec log_file_name() -> string().
log_file_name() ->
  %% 获取前缀
  ConfigPrefix = get_file_name_prefix(),
  Prefix = case ConfigPrefix of
             [] -> [];
             _ -> lists:concat([ConfigPrefix, "."])
           end,
  %% 根据当前时间生成默认的文件名，格式示例：2022-05-19-15
  Time = current_time_string(),
  %% 拼接路径path
  Directory = lists:concat([get_directory(), "/"]),
  FileName = lists:concat([Prefix, "log.", Time]),
  %% 根据初始化的文件名、文件夹，默认的文件下标，来生成一个可用的文件名字
  ValidateName = validate_file_name(Directory, FileName, 0),
  %% 构造完整的文件路径
  string:concat(Directory, ValidateName).

%% 根据当前时间生成文件名的后缀，格式示例：2022-05-19-15
-spec current_time_string() -> string().
current_time_string() ->
  {{Y, M, D}, {H, _Minute, _Second}} = time_format(os:timestamp()),
  ConfigRotateMode = get_rotate_mode(),
  Daily = rotate_mode_daily(),
  Hour = rotate_mode_hour(),
  case ConfigRotateMode of
    Daily -> lists:concat([Y, "-", M, "-", D]);
    Hour -> lists:concat([Y, "-", M, "-", D, "-", H])
  end.

validate_file_size(Directory, NewFileName) ->
  FilePath = string:concat(Directory, NewFileName),
  %% 获取文件信息
  {ok, Facts} = file:read_file_info(FilePath),
  %% 获取文件大小
  Size = Facts#file_info.size,
  ConfigSize = get_file_size(),
  if
    ConfigSize > 0 ->
      if
        ConfigSize * 1024 * 1024 =< Size -> false;
        true -> true
      end;
    true -> true
  end.

validate_file_name(Directory, FileName, Index) ->
  NewFileName = if
                  Index == 0 -> FileName;
                  true -> lists:concat([FileName, "_", Index])
                end,
  case search_file(Directory, NewFileName) of
    [] -> NewFileName;
    _Result ->
      %% 如果size超出，就在文件后面追加数字，生成一个新的文件名字，重复进行文件名合法性的判断
      case validate_file_size(Directory, NewFileName) of
        true -> NewFileName;
        false -> validate_file_name(Directory, FileName, Index + 1)
      end
  end.

%% 在指定的文件夹内搜索是否存在某一个文件，存在就返回文件名，不存在返回空字符串
-spec search_file(string(), string()) -> string().
search_file(Directory, FileName) ->
  case file:list_dir(Directory) of
    {ok, Files} ->
      find_file(Files, FileName);
    {error, _Files} -> []
  end.

%% 查找数组中是否存在某个文件
-spec find_file([], string()) -> string().
find_file([], _FileName)->
  [];
find_file([H | T],FileName)->
  case H == FileName of
    true -> FileName;
    _ -> find_file(T, FileName)
  end.

%% 获取格式化后的时间
-spec time_format(erlang:timestamp()) -> calendar:datetime1970().
time_format(Time) ->
  {M, S, _Micro} = Time,
  DT = calendar:gregorian_seconds_to_datetime(M * 1000000 + S +
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})),
  calendar:universal_time_to_local_time(DT).

%% 立即发送。暂时不需要调用
-spec flush() -> _.
flush() ->
  %% 立刻上报
  write_to_file().

%% 关闭SDK
-spec close() -> _.
close() ->
  %% 立刻上报
  write_to_file(),
  %% 删除ETS表
  ets:delete(?CONFIG_TABLE),
  ets:delete(?EVENT_TABLE).

%% 在 ets 中根据key设置value
-spec set_value2ets(string(), string()) -> _.
set_value2ets(Key, Value) ->
  try
    ets:insert(?CONFIG_TABLE, {Key, Value})
  catch
    error:_ -> io:format("thinking data error: set value to ets failed key:~p .~n", [Key])
  end.

%% 在 ets 中根据key查找value
-spec find_value_from_ets(string()) -> _.
find_value_from_ets(Key) ->
  try
    [{_, E}|_] = ets:lookup(?CONFIG_TABLE, Key),
    E
  catch
    error:_ -> []
  end.

%% 添加事件到缓存
-spec add_event2ets(string()) -> _.
add_event2ets(Event) ->
  try
    ets:insert(?EVENT_TABLE, {Event})
  catch
    error:_ -> io:format("[thinking data] insert event to ets failed:~p .~n", [Event])
  end.

write_to_file() ->
  %% 获取ETS 中所有缓存的数据
  Records = ets:tab2list(?EVENT_TABLE),
  if
    length(Records) > 0 ->
      %% 拿到一个可用的文件名
      FilePath = log_file_name(),
      %% 打开文件
      {ok, File} = file:open(FilePath, [read, write, append]),
      %% 写入文件
      write_list(Records, File),
      %% 关闭文件
      file:close(File);
    true -> []
  end.

%% 查找数组中是否存在某个文件
-spec write_list([], file:fd()) -> _.
write_list([], _)->
  [];
write_list([H | T], File)->
  %% 删除数据
  ets:delete_object(?EVENT_TABLE, H),
  {Temp} = H,
  %% 追加事件json字符串内容到文件
  io:format(File, "~s~n", [Temp]),
  write_list(T, File).

loop_write_to_file() ->
  write_to_file(),
  receive
  after
    %% 间隔3s
    3000 -> loop_write_to_file()
  end.