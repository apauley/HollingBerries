#!/usr/bin/env ruby

require './dsl/produce_file'
require './dsl/price_file'

class Apple < Fruit
  markup     1.4
  shelf_life 14
end

class Banana < Fruit
  markup     1.35
  shelf_life 5
end

class Berry < Fruit
  markup     1.55
  shelf_life 7
end

class DodgySupplier < Supplier
  price_adjustment -2
  shelf_life_adjustment -3
end

class PremiumSupplier < Supplier
  markup_adjustment 0.1
  round_up_price    true
end

produce_file = ProduceFile.new(File.expand_path("../../produce.csv", File.dirname(__FILE__)))
price_file = PriceFile.new(File.expand_path('pricefile.txt', File.dirname(__FILE__)))

produce = produce_file.read
price_file.write(produce)








































