%% -
%% Steven Swart's solution to the Hollingberries Convenience Store problem.
%% -

-module(hollingberries).
-import(string).
-import(dateutils,[add/3]).
-import(lists,[foreach/2]).
-import(file,[open/2,close/1,write/2]).
-import(io_lib,[format/2]).

%% A special thank you to Luke Krasnoff <luke.krasnoff@gmail.com> for sending
%% me his CSV parser, without which I don't think I could have solved this problem!
%%
-import(csv_luke).

-import(edate).
-export([process_produce/2]).

%% Function to round up to the next highest integer.
%%
ceiling(X) ->
   T = erlang:trunc(X),
   case (X - T) of
      Neg when Neg < 0 -> float(T);
      Pos when Pos > 0 -> float(T + 1);
      _ -> float(T)
   end.

%% The main function that calculates the mark-up and the sell-by date.
%%
calculate_markup_and_sellby(DeliveryDate, ProductCode, 32) -> %% Susan Windler
   MarkUpAndSellBy = calculate_markup_and_sellby(DeliveryDate, ProductCode),
   {element(1, MarkUpAndSellBy), edate:shift(element(2, MarkUpAndSellBy), -3, days), -2, 0};

calculate_markup_and_sellby(DeliveryDate, ProductCode, 101) -> %% Togetherness Tshabalala
   MarkUpAndSellBy = calculate_markup_and_sellby(DeliveryDate, ProductCode),
   {element(1, MarkUpAndSellBy), edate:shift(element(2, MarkUpAndSellBy), -3, days), -2, 0};

calculate_markup_and_sellby(DeliveryDate, ProductCode, 204) ->  %% Karel Visser
   MarkUpAndSellBy = calculate_markup_and_sellby(DeliveryDate, ProductCode),
   {(0.1 + element(1, MarkUpAndSellBy)), element(2, MarkUpAndSellBy), 0, 1};

calculate_markup_and_sellby(DeliveryDate, ProductCode, 219) -> %% Promise Mashangu
   MarkUpAndSellBy = calculate_markup_and_sellby(DeliveryDate, ProductCode),
   {(0.1 + element(1, MarkUpAndSellBy)), element(2, MarkUpAndSellBy), 0, 1};

calculate_markup_and_sellby(DeliveryDate, ProductCode, _SupplierId) ->
   calculate_markup_and_sellby(DeliveryDate, ProductCode).

%% The sub-function for calculating the mark-up and the sell-by date.
%%
calculate_markup_and_sellby(DeliveryDate, ProductCode) when ProductCode >= 1000, ProductCode =< 1099 -> %% Other fruit
   {0.5, edate:shift(DeliveryDate, 7, days), 0, 0};

calculate_markup_and_sellby(DeliveryDate, ProductCode) when ProductCode >= 1100, ProductCode =< 1199 -> %% Apples
   {0.4, edate:shift(DeliveryDate, 14, days), 0, 0};

calculate_markup_and_sellby(DeliveryDate, ProductCode) when ProductCode >= 1200, ProductCode =< 1299 -> %% Bananas
   {0.35, edate:shift(DeliveryDate, 5, days), 0, 0};

calculate_markup_and_sellby(DeliveryDate, ProductCode) when ProductCode >= 1300, ProductCode =< 1399 -> %% Berries
   {0.55, edate:shift(DeliveryDate, 7, days), 0, 0};

calculate_markup_and_sellby(DeliveryDate, ProductCode) when ProductCode >= 1400, ProductCode =< 1999 -> %% Other fruit
   {0.5, edate:shift(DeliveryDate, 7, days), 0, 0};

calculate_markup_and_sellby(_DeliveryDate,_ProductCode) -> "Invalid product code!".

%% This function simulates a for loop by outputting multiple labels recursively.
%%
for_loop(N, Length, _Inc, _SellPrice, _Year, _Month, _Day, _ProductDescription, _Io_device) when N >= Length -> exit;

for_loop(N, Length, _Inc, SellPrice, Year, Month, Day, ProductDescription, Io_device) ->
  %% io:format("R~8.2f~B/~2..0B/~2..0B~s~n",[SellPrice,Year,Month,Day,ProductDescription]),
   OutputLine = io_lib:format("R~8.2f~B/~2..0B/~2..0B~s~n",[SellPrice,Year,Month,Day,ProductDescription]),
   io:format(OutputLine),
   file:write(Io_device,OutputLine),
   for_loop(N+_Inc, Length, _Inc, SellPrice, Year, Month, Day, ProductDescription, Io_device).

%% The main processing function, takes the input produce CSV filename
%% and the output price list filename as parameters.
%%
process_produce(InputFile, OutputFile) ->

   Produce = tl(csv_luke:parse_file(InputFile)), %% Strip the header

   {Status, Io_device} = file:open(OutputFile, write),
   if
      Status == error -> error("Could not open output file for writing!", {InputFile, OutputFile});
	  true-> continue
   end,

   %% The Printer fun that processes the produce list
   Printer = fun(X) ->

      CostPrice = element(1,string:to_integer(element(5,X))),
      DeliveryDate = edate:string_to_date(element(4,X)),
      ProductCode = element(1,string:to_integer(element(2,X))),
      SupplierId = element(1,string:to_integer(element(1,X))),
      MarkupAndDate = calculate_markup_and_sellby(DeliveryDate, ProductCode, SupplierId),
      IntermediateSellPrice = CostPrice * (1 + element(1, MarkupAndDate))/100,
      if
         IntermediateSellPrice + element(3, MarkupAndDate) < 0 ->
	     SellPriceBeforeRounding  = 0.0;
	  true ->
             SellPriceBeforeRounding  = IntermediateSellPrice + element(3, MarkupAndDate)
      end,
      if
         element(4, MarkupAndDate) > 0 ->
	     SellPrice = ceiling(SellPriceBeforeRounding);
	  true ->
             SellPrice = SellPriceBeforeRounding
      end,
      Year = element(1,element(2,MarkupAndDate)),
      Month = element(2,element(2,MarkupAndDate)),
      Day = element(3,element(2,MarkupAndDate)),
      ProductDescription = string:sub_string(element(3,X),1,31),
      Units = element(1,string:to_integer(element(6,X))),

      for_loop(0, Units, 1, SellPrice, Year, Month, Day, ProductDescription, Io_device)

   end,

   lists:foreach(Printer,Produce),

   close(Io_device).
