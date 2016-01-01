package name.sccu.csv4s

import java.io.FileInputStream
import java.text.ParseException

import scala.io.Source

class CsvParserTest extends org.scalatest.FlatSpec {

  "CsvParser" should "parse naive csv string." in {
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

  ignore should "parse a sample file." in {
    val path = "/Users/sccu/Downloads/pois_by_themes/관광음식업정보_관광식당(수시).csv"
    val parser: CsvParser = CsvParser(new FileInputStream(path), "UTF-8")
    val header = parser.headerOption.get
    parser.map(t => (header zip t).map(col => s"${col._1}:${col._2}")).take(5).foreach(println)
  }

  it should "not parse a wrong csv." in {
    val src = Source.fromString("a,b\"\"b,c")
    val parser: CsvParser = CsvParser(src, noHeader = true)
    intercept[ParseException] {
      parser.next
    }
  }
}
