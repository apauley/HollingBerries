require 'csv'
require 'date'

class Product
  attr_reader :supplier_id, 
              :product_code,
              :description,
              :delivery_date,
              :cost_price,
              :unit_count

  attr_accessor :markup, 
                :shelf_life

  def initialize(supplier_id, product_code, description, delivery_date, cost_price, unit_count)
    @supplier_id = supplier_id.to_i
    @product_code = product_code.to_i
    @description = description
    @delivery_date = Date.parse delivery_date
    @cost_price = cost_price.to_i
    @unit_count = unit_count.to_i

    @markup = 0.5
    @shelf_life = 7
  end

  def selling_price
    cost_price * (1 + markup)
  end

  def sell_by
    delivery_date + shelf_life
  end

  def short_description
    description[0..30]
  end
end

module Reader
  def read(filename)
    products = []
    # We're ignoring the CSV file header
    # Better would be if we read the header and matched it to the columns we're interested in
    first = true
    CSV.foreach(filename) do |row|
      products << Product.new(*row) unless first
      first = false
    end
    products
  end
end

module RuleProcessor
  def self.included(host)
    host.extend ClassMethods
  end

  module ClassMethods
    def has_rules(*rules_arr)
      rules.concat rules_arr
    end

    def rules
      @rules ||= []
    end
  end

  def apply_rules!(products)
    products.each do |product|
      self.class.rules.each do |rule|
        rule.apply! product if rule.matches? product
      end
    end
  end
end

class Rule
  class << self
    def product_codes(from, to)
      @product_codes_from = from
      @product_codes_to = to
    end

    def shelf_life(days = nil)
      @shelf_life = days if days
      @shelf_life
    end

    def markup(perc = nil)
      @markup = perc if perc
      @markup
    end

    def matches?(product)
      product.product_code >= @product_codes_from && product.product_code <= @product_codes_to
    end

    def apply!(product)
      product.markup = @markup if @markup
      product.shelf_life = @shelf_life if @shelf_life
    end
  end
end

class FruitRule < Rule
  product_codes 1100, 1199
  markup 0.4
  shelf_life 14
end

class AppleRule < Rule
  product_codes 1100, 1199
  markup 0.4
  shelf_life 14
end

class BananaRule < Rule
  product_codes 1200, 1299
  markup 0.35
  shelf_life 5
end

class BerryRule < Rule
  product_codes 1300, 1399
  markup 0.55
end

class SupplierRule
  def self.for_suppliers(*ids)
    suppliers.concat ids
    self
  end

  def self.suppliers
    @suppliers ||= []
  end

  def self.matches?(product)
    suppliers.include? product.supplier_id 
  end
end

class TroubleSupplierRule < SupplierRule
  def self.apply!(product)
    product.extend Overrides
  end

  module Overrides
    def selling_price
      [cost_price * (1 + markup) - 200, 0].max
    end

    def sell_by
      delivery_date + shelf_life - 3
    end
  end
end

class PremiumProduceRule < SupplierRule
  def self.apply!(product)
    product.extend Overrides
  end

  module Overrides
    def selling_price
      (cost_price * (1 + markup + 0.1) / 100).ceil * 100
    end
  end
end

module Writer
  def write(filename, products)
    File.open(filename, 'w') do |file|
      products.each do |product|
        product.unit_count.times { file.puts "R#{'% 8.2f' % (product.selling_price / 100.0)}#{product.sell_by.strftime('%Y/%m/%d')}#{product.short_description}" }
      end
    end
  end
end

class LabelPrinter
  include Reader
  include RuleProcessor
  include Writer

  has_rules FruitRule,
            AppleRule,
            BananaRule,
            BerryRule,
            TroubleSupplierRule.for_suppliers(32, 101),
            PremiumProduceRule.for_suppliers(219, 204)

  def process(input_filename, output_filename)
    products = read input_filename
    apply_rules! products
    write output_filename, products
  end
end

LabelPrinter.new.process '../../produce.csv', 'pricefile.txt'




