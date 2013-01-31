class Supplier

  def round_up_price?
    self.class.class_variable_get(:@@round_up)
  rescue NameError
    false
  end

  def markup_adjustment
    self.class.class_variable_get(:@@markup)
  rescue NameError
    0
  end

  def price_adjustment
    self.class.class_variable_get(:@@price)
  rescue NameError
    0
  end

  def shelf_life_adjustment
    self.class.class_variable_get(:@@shelf_life)
  rescue NameError
    0
  end

  class << self

    def load(row)
      if [32,101].include?(row[0].to_i)
        DodgySupplier.new
      elsif [219,204].include?(row[0].to_i)
        PremiumSupplier.new
      else
        Supplier.new
      end
    end

    def round_up_price(val)
      val = true if val # make sure its a bool
      class_variable_set(:@@round_up, val)
    end

    def markup_adjustment(val)
      class_variable_set(:@@markup, val)
    end

    def price_adjustment(val)
      class_variable_set(:@@price, val)
    end

    def shelf_life_adjustment(val)
      class_variable_set(:@@shelf_life, val)
    end

  end

end

