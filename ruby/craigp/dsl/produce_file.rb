require 'csv'

class ProduceFile

  attr_reader :path

  def initialize(path)
    @path = path
    raise StandardError, "Produce file does not exist: #{path}" unless File.exists?(path)
  end

  def read
    rows = CSV.read(path)
    rows.shift # remove header row
    [].tap do |produce|
      rows.each do |row|
        supplier = Supplier.load(row)
        fruit = Fruit.load(supplier, row)
        produce << fruit
      end
    end
  end

end