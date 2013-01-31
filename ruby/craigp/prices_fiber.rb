#!/usr/bin/env ruby

require 'csv'
require 'fiber'

config_fiber = Fiber.new do |calling_fiber, product_code|
  loop do
    mark_up, shelf_life = case product_code.to_i
    when 1100..1199; [1.4, 14]
    when 1200..1299; [1.35, 5]
    when 1300..1399; [1.55, 7]
    else; [1.5, 7]; end
    calling_fiber, product_code = calling_fiber.transfer([mark_up, shelf_life])
  end
end

pricefile_fiber = Fiber.new do |row|
  loop do
    sell_price, sell_by_date = row[4].to_f / 100, Date.parse(row[3])
    mark_up, shelf_life = config_fiber.transfer(Fiber.current, row[1])
    sell_price = %w(219 204).include?(row[0]) ? (sell_price * (mark_up + 0.1)).ceil : sell_price * mark_up
    (shelf_life -= 3 and sell_price -= 2) if %w(32 101).include?(row[0])
    row = Fiber.yield([].tap do |lines|
      row[5].to_i.times do
        lines << "R#{sprintf("%0.2f", [sell_price,0].max).rjust(8)}#{(sell_by_date + shelf_life).strftime("%Y/%m/%d")}#{row[2][0..30]}"
      end
    end)
  end
end

CSV.read(File.join(File.dirname(__FILE__), "../../produce.csv")).tap do |reader|
  reader.shift
  File.open('pricefile.txt','w') do |out|
    reader.each { |row| out.puts pricefile_fiber.resume(row) }
    out.close
  end
end
