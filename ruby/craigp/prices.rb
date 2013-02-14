#!/usr/bin/env ruby

require 'csv'

CSV.read(File.expand_path("../../produce.csv", File.dirname(__FILE__))).tap do |reader|
  reader.shift
  File.open(File.expand_path('pricefile.txt', File.dirname(__FILE__)),'w') do |out|
    reader.each do |row|
      puts row
      sell_price, sell_by_date = row[4].to_f / 100, Date.parse(row[3])
      mark_up, shelf_life = case row[1].to_i
      when 1100..1199; [1.4, 14]
      when 1200..1299; [1.35, 5]
      when 1300..1399; [1.55, 7]
      else; [1.5, 7]; end
      sell_price = %w(219 204).include?(row[0]) ? (sell_price * (mark_up + 0.1)).ceil : sell_price * mark_up
      (shelf_life -= 3 and sell_price -= 2) if %w(32 101).include?(row[0])
      row[5].to_i.times do
        out.puts "R#{sprintf("%0.2f", [sell_price,0].max).rjust(8)}#{(sell_by_date + shelf_life).strftime("%Y/%m/%d")}#{row[2][0..30]}"
      end
    end
    out.close
  end
end