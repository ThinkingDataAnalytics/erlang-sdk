%%%-------------------------------------------------------------------
%%% @author ThinkingData
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 7月 2022 12:05
%%%-------------------------------------------------------------------
-module(ta_utils).
-author("ThinkingData").

%% API
-export([format_time/1]).

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
