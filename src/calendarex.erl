%%%----------------------------------------------------------------------
%%%
%%% @author chenduo
%%% @date 2014-12-05
%%% @doc calendar的扩展
%%%
%%%----------------------------------------------------------------------
-module(calendarex).
-export([date_string/0, date_string/1, time_string/0, time_string/1]).
-export([next_day/1, next_weekday/2, next_weekday/3, secsshift/2, dayshift/2, secsdiff/2, daysdiff/2]).
-export([unixtime_to_localtime/1, localtime_to_unixtime/1]).
-export([midnight/1]).
-export([local_timezone/0, local_utc_offset/0, offset_to_timezone/1]).
-export([parse_date/1]).

%% @doc 根据当前日期返回20130602这样的字符串
date_string() ->
    date_string(game_misc:now_date()).
date_string({Y, M, D}) ->
    lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B", [Y, M, D])).

%% @doc 当前时间字符串形式
time_string() ->
    time_string(game_misc:local_time()).
time_string({{Y, M ,D}, {H, Mi, S}}) ->
    lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B-"
        "~2.10.0B~2.10.0B~2.10.0B", [Y, M, D, H, Mi, S])).

%% @doc 必须是"20180101"这样的格式, 返回值{Y, M, D}
parse_date(<<YearB:4/binary, MonthB:2/binary, DayB:2/binary>>) ->
    Year = binary_to_integer(YearB),
    Month = binary_to_integer(MonthB),
    Day = binary_to_integer(DayB),
    {Year, Month, Day}.

%% @doc 加24小时
next_day({H, M, S}) ->
    {H + 24, M, S}.

%% @doc 取距离指定日期最近的星期N, 可能返回参数里的日期
next_weekday(Date, Weekday) ->
    W = calendar:day_of_the_week(Date),
    next_weekday(Date, W, Weekday).
next_weekday(Date, DateWeekday, Weekday) ->
    DaysDiff = (Weekday - DateWeekday + 7) rem 7,
    dayshift(Date, DaysDiff).

%% @doc 取指定日期N天后的日期
dayshift(DateTime, 0) ->
    DateTime;
dayshift({Date, Time}, Shift) ->
    {dayshift(Date, Shift), Time};
dayshift(Date, Shift) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + Shift).

%% @doc 取指定时间N秒后的时间
secsshift(DateTime, 0) ->
    DateTime;
secsshift(DateTime, Shift) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime) + Shift).

%% @doc 计算两个时间的秒差
secsdiff(DateTimeA, DateTimeB) ->
    calendar:datetime_to_gregorian_seconds(DateTimeA) -
        calendar:datetime_to_gregorian_seconds(DateTimeB).

%% @doc 计算两个日期的天数差
daysdiff(DateA, DateB) ->
    calendar:date_to_gregorian_days(DateA) - calendar:date_to_gregorian_days(DateB).

%% @doc 计算UTCTimestamp对应的当地日期时间
unixtime_to_localtime(Timestamp) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    UTCDateTime = calendar:gregorian_seconds_to_datetime(Timestamp + Epoch),
    calendar:universal_time_to_local_time(UTCDateTime).

%% @doc 计算当地日期时间对应的UTCTimestamp
localtime_to_unixtime(LocalTime) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    UTCDateTime = hd(calendar:local_time_to_universal_time_dst(LocalTime)),
    calendar:datetime_to_gregorian_seconds(UTCDateTime) - Epoch.

%% @doc 计算指定时间戳当天00:00:00的时间戳
midnight(Timestamp) ->
    {Date, _} = calendarex:unixtime_to_localtime(Timestamp),
    calendarex:localtime_to_unixtime({Date, {0, 0, 0}}).

local_timezone() ->
    offset_to_timezone(local_utc_offset()).

local_utc_offset() ->
    Utc = calendar:universal_time(),
    calendarex:secsdiff(calendar:universal_time_to_local_time(Utc), Utc).

offset_to_timezone(Offset) when Offset >= 0 ->
    erlang:iolist_to_binary(io_lib:format("+~w:00", [Offset div 3600]));
offset_to_timezone(Offset) ->
    erlang:iolist_to_binary(io_lib:format("~w:00", [Offset div 3600])).


