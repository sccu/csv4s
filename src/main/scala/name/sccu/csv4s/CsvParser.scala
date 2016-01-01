package name.sccu.csv4s

import java.io.{BufferedInputStream, InputStream}
import java.text.ParseException

import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

class CsvParser(val headerOption: Option[Seq[String]], lines: Iterator[String])
  extends Iterator[Seq[String]] {

  override def hasNext: Boolean = lines.hasNext

  override def next(): Seq[String] = {
    val recordString = CsvParser.nextRow(lines)
    val result = CsvParser.parseAll(CsvParser.record, recordString)
    if (result.successful) result.get
    else throw new ParseException(s"[$recordString]", 0)
  }
}

object CsvParser extends RegexParsers with PackratParsers {
  def apply(src: Source, noHeader: Boolean = false): CsvParser = {
    val headerOption = if (noHeader) {
      None
    } else {
      val lines = src.getLines()
      val header = CsvParser.parseAll(record, CsvParser.nextRow(lines)).get
      Some(header)
    }
    new CsvParser(headerOption, src.getLines())
  }

  def apply(is: InputStream, encoding: String): CsvParser = {
    val bis = if (is.markSupported()) is else new BufferedInputStream(is)

    bis.mark(8)
    if (bis.read() != 0xEF || bis.read() != 0xBB || bis.read() != 0xBF) {
      bis.reset()
    }

    apply(Source.fromInputStream(bis, encoding))
  }

  def apply(inputStream: InputStream): CsvParser = {
    apply(Source.fromInputStream(inputStream, "UTF-8"))
  }

  private val MAX_LINE_COUNT = 100

  private def nextRow(lines: Iterator[String]) = {
    @tailrec
    def findRow(text: String, limit: Int): String =
      if (text.count(_ == '"') % 2 == 0) {
        text
      } else if (limit > 0 && lines.hasNext) {
        findRow(text + "\r\n" + lines.next(), limit - 1)
      } else {
        throw new IllegalStateException(s"Invalid csv format:\n$text")
      }
    val line = lines.next()
    findRow(line, MAX_LINE_COUNT)
  }

  private lazy val record: PackratParser[Seq[String]] = field ~! rep(SEPARATOR ~> field) ^^ {
    case f ~ l => f +: l
  }

  private lazy val field: PackratParser[String] = escaped | non_escaped

  private lazy val escaped: PackratParser[String] = s"""(?s)"(""|[^\"])*"""".r ^^ {
    _.tail.init.replace("\"\"", "\"")
  }

  private lazy val non_escaped: PackratParser[String] = parser2packrat(s"""[^\"\r\n$SEPARATOR]*""".r)

  private def SEPARATOR = ",".r
}