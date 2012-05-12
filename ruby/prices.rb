#!/usr/bin/env ruby

require 'csv'

class Product
  def self.create(product_code, description, delivery_date, cost_price, unit_count)
    if product_code >= 1000 and product_code <= 1999
      Fruit.create(product_code, description, delivery_date, cost_price, unit_count)
    elsif product_code >= 2000 and product_code <= 3999
      DairyProduct.create(product_code, description, delivery_date, cost_price, unit_count)
    elsif product_code >= 4000 and product_code <= 7999
      Meat.create(product_code, description, delivery_date, cost_price, unit_count)
    else
      raise "Bad product code: #{product_code} #{description}"
    end
  end
  def initialize(product_code, description, delivery_date, cost_price, unit_count)
  end
end

class Fruit < Product
  def self.create(product_code, description, delivery_date, cost_price, unit_count)
    if product_code >= 1100 and product_code <= 1199
      Apple.new(product_code, description, delivery_date, cost_price, unit_count)
    elsif product_code >= 1200 and product_code <= 1299
      Banana.new(product_code, description, delivery_date, cost_price, unit_count)
    else
      raise "Bad Fruit product code: #{product_code} #{description}" 
    end
  end
end

class Apple < Fruit
end
class Banana < Fruit
end

class DairyProduct < Product
  def self.create(product_code, description, delivery_date, cost_price, unit_count)
    if product_code >= 2100 and product_code <= 2199
      Milk.new(product_code, description, delivery_date, cost_price, unit_count)
    else
      raise "Bad dairy product code: #{product_code} #{description}"
    end
  end
end

class Milk < DairyProduct
end

class Meat < Product
  def self.create(product_code, description, delivery_date, cost_price, unit_count)
    MinceMeat.new(product_code, description, delivery_date, cost_price, unit_count)
  end
end

class MinceMeat < Meat
end

def to_price_file(line)
  CSV.parse(line) do |row|
    product_code  = Integer(row[1])
    description   = row[2]
    delivery_date = row[3]
    cost_price    = Integer(row[4])
    unit_count    = Integer(row[5])
    prod = Product.create(product_code, description, delivery_date, cost_price, unit_count)
    puts prod
    puts
  end
end


filename = '../produce.csv'
file = File.new(filename, 'r')

file.each_line("\n") do |row|
  if file.lineno > 1
    to_price_file(row)
  end
end
