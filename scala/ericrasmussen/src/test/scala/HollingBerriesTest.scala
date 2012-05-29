import org.scalatest.FunSuite
import HollingBerries.DateTime
import HollingBerries.ProduceRecord
import HollingBerries.ProduceRecord._
import HollingBerries.ProduceParser.{
    parse
  , int
  , desc
  , date
  , record
}

class ParserSuite extends FunSuite {

  test("ProduceParser parses an int with no comma") {
    val result = parse(int, "10")
    assert(result.get === 10)
  }

  test("ProduceParser parses an int and consumes comma") {
    val result = parse(int, "10,")
    assert(result.get === 10)
    assert(result.next.atEnd === true)
  }

  test("ProduceParser parses a quoted string and ignores commas") {
    val result = parse(desc, "\"string, quoted\"")
    assert(result.get === "string, quoted")
  }

  test("ProduceParser parses a date string") {
    val result   = parse(date, "\"2012/01/31\"")
    val expected = DateTime.fromString("2012/01/31")
    assert(result.get.toString === expected.toString)
  }

  test("ProduceParser parses a produce record") {
    val result   = parse(record, "1,2,\"Apples\",\"2012/01/31\",3,4")
    val testDate = DateTime.fromString("2012/01/31")
    val expected = new ProduceRecord(1, 2, "Apples", testDate, 3, 4)
    assert(result.get.toString === expected.toString)
  }

}

class DateTimeSuite extends FunSuite {

  test("Create a DateTime from a string") {
    val example = "2012/01/31"
    val result  = DateTime.fromString(example)
    assert(result.toString === example)
  }

  test("Add days to a DateTime object") {
    val inst = DateTime.fromString("2012/01/31")
    val inc  = inst plus 1
    assert(inc.toString === "2012/02/01")
  }

  test("Subtract days from a DateTime object") {
    val inst = DateTime.fromString("2012/01/31")
    val inc  = inst plus -1
    assert(inc.toString === "2012/01/30")
  }

}

class ProduceRecordSuite extends FunSuite {

  val sampleDate = DateTime.fromString("2012/01/31")
  val normalRec  = new ProduceRecord(1, 1200, "Bananas", sampleDate, 3, 4)
  val premiumRec = new ProduceRecord(204, 1200, "Bananas", sampleDate, 3, 4)
  val troubleRec = new ProduceRecord(32, 1200, "Bananas", sampleDate, 3, 4)

  test("ProduceRecord correctly identifies its fruitType") {
    assert(normalRec.fruitType === Bananas)
  }

  test("ProduceRecord correctly identifies its markup") {
    assert(normalRec.markup === 1.35)
  }

  test("ProduceRecord correctly identifies its sellby") {
    assert(normalRec.sellby === 5)
  }

  test("ProduceRecord correctly identifies premium") {
    assert(normalRec.isPremium === false)
    assert(premiumRec.isPremium === true)
  }

  test("ProduceRecord sets final price") {
    val expected = 3 * 1.35 / 100
    assert(normalRec.finalPrice === expected)
  }

  test("ProduceRecord sets final price for premium supplier") {
    val expected = math.ceil(3 * (1.35 + 1.1) / 100)
    assert(premiumRec.finalPrice === expected)
  }

  test("ProduceRecord sets final price for trouble supplier") {
    val expected = math.max((3 * 1.35 / 100) - 2, 0)
    assert(troubleRec.finalPrice === expected)
  }

  test("ProduceRecord correctly sets final sellby") {
    assert(normalRec.finalSellby.toString === "2012/02/05")
  }

  test("ProduceRecord sets final sellby for trouble supplier") {
    assert(troubleRec.finalSellby.toString === "2012/02/02")
  }

  test("ProduceRecord creates a price tag") {
    val single_unit = "R    0.042012/02/05Bananas"
    val expected = (1 to normalRec.units).map(_ => single_unit).mkString("\n")
    assert(normalRec.toString === expected + "\n")
  }

  test("ProduceRecord with 0 units is represented by an empty string") {
    val emptyRec = new ProduceRecord(1, 1200, "Bananas", sampleDate, 3, 0)
    assert(emptyRec.toString === "")
  }

}
