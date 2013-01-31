require './dsl/fruit'
require './dsl/fruit_formatter'

class PriceFile

  def initialize(path)
    @path = path
  end

  def write(produce)
    File.open(@path,'w') do |file|
      produce.each do |fruit|
        file.puts FruitFormatter.write(fruit)
      end
      file.close
    end
  end

end