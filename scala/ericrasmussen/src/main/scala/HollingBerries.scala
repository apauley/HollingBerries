package HollingBerries

import java.util.Calendar
import scala.util.matching.Regex
import scala.util.parsing.combinator._

object ProduceParser extends RegexParsers {
  val comma  = ","
  val quote  = "\""
  val int    = """\d+""".r <~(comma?) ^^ (_.toInt)
  val desc   = quote~> """[^\"]+""".r <~quote <~(comma?)
  val date   = quote~> """[\d/]+""".r <~quote <~(comma?) ^^ DateTime.fromString

  val record = int~int~desc~date~int~int ^^ {
    case supp~prod~desc~date~price~units =>
      new ProduceRecord(supp, prod, desc, date, price, units)
  }
}

class ProduceRecord(val supplier    : Int,    val product: Int,
                    val description : String, val date   : DateTime,
                    val priceInCents: Int,    val units  : Int) {

  import ProduceRecord._

  val fruitType = getType(product)

  val isPremium = supplier == 204 || supplier == 219

  val isTrouble = supplier == 32  || supplier == 101

  val markup = getMarkup(fruitType) + (if (isPremium) 0.1 else 0)

  val sellby = getSellby(fruitType) - (if (isTrouble) 3   else 0)

  val finalPrice  = priceInCents * markup / 100 match {
      case p if isPremium => math.ceil (p)
      case p if isTrouble => math.max  (p - 2, 0)
      case p              => p
  }

  val finalSellby = date plus sellby

  override def toString = {
    val desc = description.take(31)
    val str  = "R%8.2f%s%s".format(finalPrice, finalSellby, desc)
    if (units >= 1)
      (1 to units).map(_ => str).mkString("\n") + "\n"
    else
      ""
  }
}

class IntID(val n: Int) {
  def between(min: Int, max: Int): Boolean = n >= min && n <= max
}

object ProduceRecord {

  sealed trait Fruit
  case object Apples  extends Fruit
  case object Bananas extends Fruit
  case object Berries extends Fruit
  case object Other   extends Fruit

  implicit def toIntID(x: Int) = new IntID(x)

  val markupMap: Map[Fruit, Double] =
    Map(Apples -> 1.4, Bananas -> 1.35, Berries -> 1.55)

  val sellbyMap: Map[Fruit, Int] = Map(Apples -> 14, Bananas -> 5)

  def getMarkup(produce: Fruit)  = markupMap getOrElse (produce, 1.5)

  def getSellby(produce: Fruit)  = sellbyMap getOrElse (produce, 7)

  def getType(product: Int)      = product match {
    case p if p between (1100, 1199) => Apples
    case p if p between (1200, 1299) => Bananas
    case p if p between (1300, 1399) => Berries
    case _                           => Other
  }
}

class DateTime(date: java.util.Date) {

  override def toString = DateTime.formatter.format(date)

  def plus(days: Int): DateTime = {
    val cal = Calendar.getInstance.asInstanceOf[Calendar]
    cal setTime date
    cal add (Calendar.DATE, days)
    new DateTime(cal getTime)
  }
}

object DateTime {

  val formatter = new java.text.SimpleDateFormat("yyyy/MM/dd")

  def fromString(dt: String) = new DateTime(formatter parse dt)
}

object Main {
  import ProduceParser._

  def main(arg: Array[String]) {
    val writer = new java.io.FileWriter("pricefile.txt")
    val source = scala.io.Source.fromURL(getClass.getResource("/produce.csv"))
    val recs = source.getLines
    for (r <- recs) {
      val parsed = parse(record, r)
      if (parsed.successful)
        writer.write(parsed.get.toString)
    }
    writer.close
  }
}
