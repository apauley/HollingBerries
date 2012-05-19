-module(prices).

-export([start/0,test/0]).

-record(product, {product_type,
                  product_code,
                  supplier_id,
                  description,
                  delivery_date,
                  cost_price,
                  unit_count,
                  sell_price,
                  sell_by_date}).

-define(PREMIUM_SUPPLIER_IDS, [204,219]).
-define(UNFRESH_SUPPLIER_IDS, [32,101]).

start() ->
  {ok, InputFile}  = file:open("../../produce.csv", [read]),
  {ok, OutputFile} = file:open("pricefile.txt", [write]),
  {ok, _Header} = file:read_line(InputFile),
  ok = process_file(InputFile, OutputFile),
  ok.

process_file(InputFile, OutputFile) ->
  case file:read_line(InputFile) of
    {ok, Line} ->
      process_file(InputFile, OutputFile, Line);
    eof ->
      ok
  end.

process_file(InputFile, OutputFile, Line) ->
  to_price_file(string:strip(Line, right, $\n), OutputFile),
  process_file(InputFile, OutputFile).

to_price_file(Line, OutputFile) ->
  Fields = csv:parse_line(Line),
  [SupplierID, ProductCode, Description, DeliveryDate, CostPrice, UnitCount] = Fields,
  Product = calculate_product(list_to_integer(SupplierID),list_to_integer(ProductCode),
                              Description, date_utils:parse_date(DeliveryDate),
                              list_to_integer(CostPrice), list_to_integer(UnitCount)),
  ok = write_pricefile(Product, OutputFile),
  ok.

calculate_product(SupplierID, ProductCode, Description, DeliveryDate, CostPrice, UnitCount) ->
  Product    = simple_product(SupplierID, ProductCode, Description, DeliveryDate, CostPrice, UnitCount),
  SellPrice  = sell_price(Product#product.product_type, SupplierID, CostPrice),
  SellByDate = sell_by_date(Product#product.product_type, SupplierID, DeliveryDate),
  Product#product{sell_price = SellPrice, sell_by_date=SellByDate}.

sell_price(ProductType, SupplierID, CostPrice) ->
  Cents = round(CostPrice + markup_cents(ProductType, SupplierID, CostPrice)),
  price_with_modifications(Cents, SupplierID).

price_with_modifications(BasePriceCents, SupplierID) ->
  MarkupCents1 = case lists:member(SupplierID, ?PREMIUM_SUPPLIER_IDS) of
                   true  -> cents_rounded_up_to_nearest_rand(BasePriceCents);
                   false -> BasePriceCents
                 end,
  MarkupCents2 = case lists:member(SupplierID, ?UNFRESH_SUPPLIER_IDS) of
                   true  -> MarkupCents1 - 200; % R2 discount
                   false -> MarkupCents1
                 end,
  max(0, MarkupCents2).

cents_rounded_up_to_nearest_rand(Cents) when is_integer(Cents) ->
  RandsPart = trunc(Cents/100),
  CentsPart = Cents rem 100,
  cents_rounded_up_to_nearest_rand(RandsPart, CentsPart).

cents_rounded_up_to_nearest_rand(RandsPart, _CentsPart=0) ->
  RandsPart * 100;
cents_rounded_up_to_nearest_rand(RandsPart, _CentsPart) ->
  (RandsPart + 1) * 100.

sell_by_date(ProductType, SupplierID, DeliveryDate) ->
  date_utils:date_add_days(DeliveryDate, shelf_days(SupplierID, ProductType)).

simple_product(SupplierID, ProductCode, Description, DeliveryDate, CostPrice, UnitCount) ->
  #product{product_type  = product_type(ProductCode),
           product_code  = ProductCode,
           supplier_id   = SupplierID,
           description   = Description,
           delivery_date = DeliveryDate,
           cost_price    = CostPrice,
           unit_count    = UnitCount}.

markup_cents(ProductType, SupplierID, CostPrice) ->
  Percentage = markup_percentage(ProductType, SupplierID),
  CostPrice * (Percentage/100.0).

markup_percentage(ProductType, SupplierID) ->
  markup_percentage(ProductType) + supplier_markup_percentage_modification(SupplierID).

markup_percentage(apple) ->
  40;
markup_percentage(banana) ->
  35;
markup_percentage(berry) ->
  55;
markup_percentage(_ProductType) ->
  50.

supplier_markup_percentage_modification(SupplierID) ->
  case lists:member(SupplierID, ?PREMIUM_SUPPLIER_IDS) of
    true  -> 10;
    false -> 0
  end.

shelf_days(_SupplierID=32, ProductType) ->
  shelf_days(ProductType) - 3;
shelf_days(_SupplierID, ProductType) ->
  shelf_days(ProductType).

shelf_days(apple)        -> 14;
shelf_days(banana)       -> 5;
shelf_days(_ProductType) -> 7.

product_type(ProductCode) when ((ProductCode >= 1100) and (ProductCode =< 1199)) ->
  apple;
product_type(ProductCode) when ((ProductCode >= 1200) and (ProductCode =< 1299)) ->
  banana;
product_type(ProductCode) when ((ProductCode >= 1300) and (ProductCode =< 1399)) ->
  berry.

label_sell_price(SellPriceCents) when is_integer(SellPriceCents) ->
  RandsPart = io_lib:format("~5.. B", [trunc(SellPriceCents/100)]),
  CentsPart = io_lib:format("~2..0B", [SellPriceCents rem 100]),
  "R"++ lists:flatten(RandsPart ++ "." ++ CentsPart).

write_pricefile(Product, OutputFile) ->
  LabelSellPrice = label_sell_price(Product#product.sell_price),
  LabelSellByDate  = date_utils:date_to_string(Product#product.sell_by_date),
  LabelDescription = string:substr(Product#product.description, 1, 31),
  Line = LabelSellPrice ++ LabelSellByDate ++ LabelDescription ++ "\n",
  Lines = [Line || _Count <- lists:seq(1,Product#product.unit_count)],
  ok = file:write(OutputFile, Lines).

test() ->
  %% http://armstrongonsoftware.blogspot.com/2009/01/micro-lightweight-unit-testing.html
  test_sell_price(),
  test_sell_price_premium_supplier(),
  test_sell_price_unfresh_supplier(),
  test_label_sell_price(),
  ok.

test_sell_price() ->
  1400 = sell_price(_ProductType=apple, SupplierID=0, CostPrice=1000),
  1350 = sell_price(banana, SupplierID, CostPrice),
  1550 = sell_price(berry,  SupplierID, CostPrice),
  1500 = sell_price(other,  SupplierID, CostPrice),
  0    = sell_price(other,  SupplierID, 0),
  ok.

test_sell_price_premium_supplier() ->
  [SupplierID|_] = ?PREMIUM_SUPPLIER_IDS,
  1600 = sell_price(_ProductType=apple, SupplierID, _CostPrice=1010),
  1500 = sell_price(_ProductType=apple, SupplierID, 1000),
  ok.

test_sell_price_unfresh_supplier() ->
  [SupplierID|_] = ?UNFRESH_SUPPLIER_IDS,
  1200 = sell_price(_ProductType=apple, SupplierID, _CostPrice=1000),
  70   = sell_price(banana, SupplierID, 200),
  0    = sell_price(berry,  SupplierID, 0),
  ok.

test_label_sell_price() ->
  "R   17.23" = label_sell_price(_Cents=1723),
  "R  123.04" = label_sell_price(12304),
  "R99999.99" = label_sell_price(9999999),
  "R    1.23" = label_sell_price(123),
  "R    1.20" = label_sell_price(120),
  "R    0.23" = label_sell_price(23),
  "R    0.01" = label_sell_price(1),
  "R    0.00" = label_sell_price(0),
  ok.
