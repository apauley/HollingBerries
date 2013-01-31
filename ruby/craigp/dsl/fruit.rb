require './dsl/produce'
require './dsl/supplier'

class Fruit < Produce

  attr_reader :supplier

  def initialize(supplier, row)
    @supplier = supplier
    super(row)
  end

  def selling_price
    price = [(cost_price * (markup + supplier.markup_adjustment)) + supplier.price_adjustment, 0].max
    price = price.ceil if supplier.round_up_price?
    price
  end

  def sell_by_date
    delivery_date + shelf_life + supplier.shelf_life_adjustment
  end

  class << self

    def load(supplier, row)
      return Apple.new(supplier, row) if is_apple?(row[1])
      return Banana.new(supplier, row) if is_banana?(row[1])
      return Berry.new(supplier, row) if is_berry?(row[1])
      return Fruit.new(supplier, row)
    end

    def is_apple?(product_code)
      (1100..1199).include?(product_code.to_i)
    end

    def is_banana?(product_code)
      (1200..1299).include?(product_code.to_i)
    end

    def is_berry?(product_code)
      (1300..1399).include?(product_code.to_i)
    end

  end

end

