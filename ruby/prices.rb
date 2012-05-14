#!/usr/bin/env ruby

require 'csv'
require 'date'

class Product
  def self.create(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    if product_code >= 1000 and product_code <= 1999
      Fruit.create(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    else
      raise "Bad product code: #{product_code} #{description}"
    end
  end
  
  def initialize(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    @supplier_id   = supplier_id
    @product_code  = product_code
    @description   = description
    @delivery_date = delivery_date
    @cost_price    = cost_price
    @unit_count    = unit_count
  end

  def shelf_days
    7
  end

  def sell_by_date
    if @supplier_id == 32
      days_to_add = shelf_days - 3
    else
      days_to_add = shelf_days
    end
    @delivery_date + days_to_add
  end

  def markup_percentage
    50
  end

  def markup
    @cost_price*(markup_percentage/100.0)
  end

  def sell_price_in_rands
    (@cost_price + markup)/100.0
  end

  def label_sell_price
    'R' + "% 8.2f" % sell_price_in_rands
  end

  def label_sell_by_date
    sell_by_date.strftime('%Y/%m/%d')
  end

  def label_description
    @description[0, 31]
  end
  
  def write_pricefile(file)
    @unit_count.times {file.puts label_sell_price + label_sell_by_date + label_description}
  end
end

class Fruit < Product
  def self.create(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    if product_code >= 1100 and product_code <= 1199
      Apple.new(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    elsif product_code >= 1200 and product_code <= 1299
      Banana.new(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    elsif product_code >= 1300 and product_code <= 1399
      Berry.new(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    else
      raise "Bad Fruit product code: #{product_code} #{description}" 
    end
  end
end

class Apple < Fruit
  def markup_percentage
    40
  end
  def shelf_days
    14
  end
end

class Banana < Fruit
  def markup_percentage
    35
  end
  def shelf_days
    5
  end
end

class Berry < Fruit
  def markup_percentage
    55
  end
end


def to_price_file(line, file)
  CSV.parse(line) do |row|
    supplier_id   = Integer(row[0])
    product_code  = Integer(row[1])
    description   = row[2]
    delivery_date = Date.parse(row[3])
    cost_price    = Integer(row[4])
    unit_count    = Integer(row[5])
    product = Product.create(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    product.write_pricefile(file)
  end
end

def main()
  inputfile  = File.new('../produce.csv', 'r')
  outputfile = File.new('pricefile.txt', 'w')

  inputfile.each_line("\n") do |line|
    if inputfile.lineno > 1
      to_price_file(line, outputfile)
    end
  end
end

main()
