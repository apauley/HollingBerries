-module(date_utils).

-export([parse_date/1,
         date_add_days/2,
         date_to_string/1]).

parse_date(DateString) ->
  Tokens = string:tokens(DateString, "/"),
  [Year, Month, Day] = [list_to_integer(Str) || Str <- Tokens],
  {Year, Month, Day}.

date_add_days(Date, Days) ->
  GregDays = calendar:date_to_gregorian_days(Date),
  calendar:gregorian_days_to_date(GregDays+Days).

date_to_string(Date) ->
  {Y, M, D} = Date,
  Year  = string:right(integer_to_list(Y), 4, $0),
  Month = string:right(integer_to_list(M), 2, $0),
  Day   = string:right(integer_to_list(D), 2, $0),
  io_lib:format("~s/~s/~s", [Year,Month,Day]).
