#!/usr/bin/env ruby

require 'csv'
require 'date'

PremiumSupplierIDs = [204,219]
UnfreshSupplierIDs = [32,101]

class Product
  def self.create(supplier, product_code, description, delivery_date, cost_price, unit_count)
    if product_code >= 1000 and product_code <= 1999
      Fruit.create(supplier, product_code, description, delivery_date, cost_price, unit_count)
    else
      raise "Bad product code: #{product_code} #{description}"
    end
  end
  
  def initialize(supplier, product_code, description, delivery_date, cost_price, unit_count)
    @supplier      = supplier
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
    @delivery_date + shelf_days + @supplier.shelf_days_modifier
  end

  def product_markup_percentage
    50
  end

  def markup_percentage
    product_markup_percentage + @supplier.markup_percentage_modifier
  end

  def markup
    @cost_price*(markup_percentage/100.0)
  end

  def sell_price
    base_sell_price = @cost_price + markup
    @supplier.modify_sell_price(base_sell_price)
  end

  def sell_price_in_rands
    sell_price/100.0
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
  def self.create(supplier, product_code, description, delivery_date, cost_price, unit_count)
    if (1100..1199).include?(product_code)
      Apple.new(supplier, product_code, description, delivery_date, cost_price, unit_count)
    elsif (1200..1299).include?(product_code)
      Banana.new(supplier, product_code, description, delivery_date, cost_price, unit_count)
    elsif (1300..1399).include?(product_code)
      Berry.new(supplier, product_code, description, delivery_date, cost_price, unit_count)
    else
      raise "Bad Fruit product code: #{product_code} #{description}" 
    end
  end
end

class Apple < Fruit
  def product_markup_percentage
    40
  end
  def shelf_days
    14
  end
end

class Banana < Fruit
  def product_markup_percentage
    35
  end
  def shelf_days
    5
  end
end

class Berry < Fruit
  def product_markup_percentage
    55
  end
end

class Supplier
  def self.create(supplier_id)
    if PremiumSupplierIDs.include?(supplier_id)
      PremiumSupplier.new(supplier_id)
    elsif UnfreshSupplierIDs.include?(supplier_id)
      UnfreshSupplier.new(supplier_id)
    else
      self.new(supplier_id)
    end
  end
  
  def initialize(supplier_id)
    @supplier_id = supplier_id
  end

  def id
    @supplier_id
  end

  def shelf_days_modifier
    0
  end

  def modify_sell_price(sell_price)
    sell_price
  end

  def markup_percentage_modifier
    0
  end
end

class PremiumSupplier < Supplier
  def markup_percentage_modifier
    10
  end

  def modify_sell_price(sell_price)
    (sell_price/100.0).ceil * 100
  end
end

class UnfreshSupplier < Supplier
  def shelf_days_modifier
    -3
  end

  def modify_sell_price(sell_price)
    [0, sell_price - 200].max
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
    
    supplier = Supplier.create(supplier_id)
    product  = Product.create(supplier, product_code, description, delivery_date, cost_price, unit_count)
    product.write_pricefile(file)
  end
end

def main()
  inputfile  = File.new('../../produce.csv', 'r')
  outputfile = File.new('pricefile.txt', 'w')

  inputfile.each_line("\n") do |line|
    if inputfile.lineno > 1
      to_price_file(line, outputfile)
    end
  end
end

main()
