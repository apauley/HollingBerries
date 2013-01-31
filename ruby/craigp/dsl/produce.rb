require 'bigdecimal'

class Produce

  DEFAULT_MARKUP = 1.5
  DEFAULT_SHELF_LIFE = 7

  def initialize(row)
    @row = row
  end

  def number_of_units
    @number_of_units ||= @row[5].to_i
  end

  def cost_price
    @cost_price ||= BigDecimal(@row[4]) / 100
  end

  def delivery_date
    @delivery_date ||= Date.parse(@row[3])
  end

  def description
    @description ||= @row[2]
  end

  def markup
    self.class.class_variable_get(:@@markup)
  rescue NameError
    DEFAULT_MARKUP
  end

  def shelf_life
    self.class.class_variable_get(:@@shelf_life)
  rescue NameError
    DEFAULT_SHELF_LIFE
  end

  class << self

    def markup(amount)
      class_variable_set(:@@markup, amount)
    end

    def shelf_life(days)
      class_variable_set(:@@shelf_life, days)
    end

  end

end