package name.sccu.csv4s

import scala.io.Source

class CsvParserTest extends org.scalatest.FlatSpec {

  "CsvParser" should "parse trivial csv string." in {
    val csv = Source.fromString("a,b,c")
    val cols = CsvParser(csv, noHeader = true).next()
    assert(cols.size == 3)
  }

  it should "parse csv starting with an empty field." in {
    val csv = Source.fromString(",b,c")
    val cols1 = CsvParser(Source.fromString(",b,c"), noHeader = true).next()
    assert(cols1.size == 3)
  }

  it should "parse csv ending with an empty field." in {
    val cols3 = CsvParser(Source.fromString("a,b,"), noHeader = true).next()
    assert(cols3.size == 3)
  }

  it should "parse csv containing doube-quoted fields." in {
    val csv = Source.fromString("a,\"b\",c")
    val cols = CsvParser(csv, noHeader = true).next()
    assert(cols(1) == "b")
  }

  it should "parse csv containing separators in the field." in {
    val csv = Source.fromString("a,\"b,b\",\"c,c\",")
    val cols = CsvParser(csv, noHeader = true).next()
    assert(cols.size == 4)
    assert(cols(1) == "b,b")
    assert(cols(2) == "c,c")
    assert(cols(3) == "")
  }

  it should "parse csv containing line breaks in the field." in {
    val csv = Source.fromString("a,\"b\r\nb\",c")
    val cols = CsvParser(csv, noHeader = true).next()
    assert(cols.size == 3)
    assert(cols(1) == "b\r\nb")
  }

  it should "parse csv all the empty fields." in {
    val csv = Source.fromString(",,")
    val cols = CsvParser(csv, noHeader = true).next()
    assert(cols.size == 3)
    assert(cols(1) == "")
  }

  it should "accept header record." in {
    val src = Source.fromString("col1,col2,col3\n1,2,3")
    val parser: CsvParser = CsvParser(src)
    val cols = parser.next()
    assert(cols.size == 3)
    assert(cols(1) == "2")
  }

  it should "handle the end of the stream." in {
    val src = Source.fromString("a,b,c")
    val parser: CsvParser = CsvParser(src, noHeader = true)
    parser.next()
    assert(parser.hasNext == false)
    assert(parser.next.size == 1)
    assert(parser.next.size == 1)
    assert(parser.next.size == 1)
  }

  it should "access to a header list." in {
    val src = Source.fromString("col1,col2,col3\n1,2,3")
    val parser: CsvParser = CsvParser(src)
    assert(parser.headerOption.get(0) == "col1")
    assert(parser.headerOption.get(1) == "col2")
    assert(parser.headerOption.get(2) == "col3")
  }

}
