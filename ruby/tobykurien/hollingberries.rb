require 'csv'
require 'date'

# map the product codes to markup percentages
@default_markup = 50
@markups_for_codes = {
    (1100..1199) => 40,
    (1200..1299) => 35,
    (1300..1399) => 55
}

# map the product codes to sell by dates (in days)
@default_sell_by = 7
@sell_by_for_codes = {
    (1100..1199) => 14,
    (1200..1299) => 5
}

# supplier ratings
@trouble_suppliers = [ 32, 101 ]
@premium_suppliers = [ 219, 204 ]

# get the markup for product
def get_markup(code) 
    @markups_for_codes.each do |key, value| 
        return value/100.00 if key.include?(code.to_i)
    end    
    @default_markup/100.00
end

# adjust the base price for supplier
def adjusted_price(supplier_code, markup, cost_price)
    # make sure return value is sane and in Rands
    def sane(value)
        return 0 if value < 0
        value / 100.0
    end
    
    # cost price is in cents which helps with calculations
    price = cost_price.to_i * (1 + markup)    
    return sane(price - 200) if @trouble_suppliers.include? supplier_code.to_i    
    return sane(cost_price.to_i * (1.1 + markup)).ceil if @premium_suppliers.include? supplier_code.to_i
    sane(price)
end

# get the base sell by date
def get_sell_by(code) 
    @sell_by_for_codes.each do |key, value| 
        return value if key.include?(code.to_i)
    end    
    @default_sell_by
end

# adjust the sell by date for supplier
def adjusted_sell_by(supplier_code, product_code, delivery_date)
    date = Date.parse(delivery_date) + get_sell_by(product_code)
    return date - 3 if @trouble_suppliers.include? supplier_code.to_i
    date
end

# Process the input file and write out the label printer file
def start()
    header = true # skip the header
    File.open("pricefile.txt", 'w') do |file|
        CSV.foreach("../../produce.csv") do |row|        
            row[5].to_i.times do
                selling_price = adjusted_price(row[0], get_markup(row[1]), row[4])
                sell_by = adjusted_sell_by(row[0], row[1], row[3])
                short_description = row[2][0..30]

                file.puts "R#{'% 8.2f' % (selling_price)}#{sell_by.strftime('%Y/%m/%d')}#{short_description}"
            end unless header
            header = false
        end
    end
end

start()

