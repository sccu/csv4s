import scala.io.Source

class CsvParserTest extends org.scalatest.FlatSpec {

  "CsvParser" should "parse trivial csv string." in {
    val csv = Source.fromString("a,b,c")
    val cols = CsvParser(csv).next()
    assert(cols.size == 3)
  }

  it should "parse csv starting with an empty field." in {
    val csv = Source.fromString(",b,c")
    val cols1 = CsvParser(Source.fromString(",b,c")).next()
    assert(cols1.size == 3)
  }

  it should "parse csv ending with an empty field." in {
    val cols3 = CsvParser(Source.fromString("a,b,")).next()
    assert(cols3.size == 3)
  }

  it should "parse csv containing doube-quoted fields." in {
    val csv = Source.fromString("a,\"b\",c")
    val cols = CsvParser(csv).next()
    assert(cols(1) == "b")
  }

  it should "parse csv containing separators in the field." in {
    val csv = Source.fromString("a,\"b,b\",\"c,c\",")
    val cols = CsvParser(csv).next()
    assert(cols.size == 4)
    assert(cols(1) == "b,b")
    assert(cols(2) == "c,c")
    assert(cols(3) == "")
  }

  it should "parse csv containing line breaks in the field." in {
    val csv = Source.fromString("a,\"b\r\nb\",c")
    val cols = CsvParser(csv).next()
    assert(cols.size == 3)
    assert(cols(1) == "b\r\nb")
  }

  it should "parse csv all the empty fields." in {
    val csv = Source.fromString(",,")
    val cols = CsvParser(csv).next()
    assert(cols.size == 3)
    assert(cols(1) == "")
  }
}
