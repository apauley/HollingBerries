class FruitFormatter

  def self.write(fruit)
    selling_price = "R#{sprintf("%0.2f", fruit.selling_price).rjust(8)}"
    sell_by_date = fruit.sell_by_date.strftime("%Y/%m/%d")
    description = fruit.description[0..30]

    [].tap do |lines|
      fruit.number_of_units.times do |unit|
        lines << "#{selling_price}#{sell_by_date}#{description}"
      end
    end
  end

end
