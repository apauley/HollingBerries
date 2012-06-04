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
    @supplier_id = supplier_id
    @product_code = product_code
    @description = description
    @cost_price = cost_price
    @unit_count = unit_count

    @markup = 0.5
    @shelf_life = 7
  end

  def selling_price
    cost_price * (1 + markup)
  end

  def sell_by
    delivery_date + shelf_life * 24 * 60 * 60
  end

  def short_description
    description[0..30]
  end
end

module Reader
  require 'csv'

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
  attr_reader :rules
  def self.included
    @rules = []
  end

  def add_rule(rule)
    @rules << rule
  end

  def process(products)
    products.each do |product|
      @rules.each do |rule|
        rule.apply! product if rule.matches? product
      end
    end
  end
end

class Rule
  class << self
    attr_reader :product_codes_from,
                :product_codes_to,
                :markup,
                :shelf_life

    def product_codes(from, to)
      @product_codes_from = from
      @product_codes_to = to
    end

    def shelf_life(days)
      @shelf_life = days
    end

    def markup(perc)
      @markup = perc
    end

    def matches?(product)
      product_code >= product_codes_from && product_code <= product_codes_to
    end

    def apply!(product)
      product.markup = markup if markup
      product.shelf_life = shelf_life if shelf_life
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

class TroubleSupplierRule < Rule
  TROUBLE_SUPPLIER_IDS = [ 32, 101 ]

  def self.matches?(product)
    TROUBLE_SUPPLIER_IDS.include? product.supplier_id 
  end

  def self.apply!(product)
    product.include self
  end

  def selling_price
    cost_price * (1 + markup) - 2
  end

  def sell_by
    delivery_date + (shelf_life - 3) * 24 * 60 * 60
  end
end

class PremiumProduceRule
  def
end

class LabelPrinter
  include Reader
  include RuleProcessor

  def process(input_filename, output_filename)
  end
end

puts Reader.read('../../produce.csv').inspect