package name.sccu.csv4s

import java.io.{BufferedInputStream, InputStream}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class CsvParser(val headerOption: Option[Seq[String]], lines: Iterator[String]) extends Iterator[Seq[String]] {

  override def hasNext: Boolean = lines.hasNext

  override def next(): Seq[String] = {
    val row = CsvParser.nextRow(lines)
    val result = CsvParser.parseAll(CsvParser.record, row)
    val fields = result.get
    fields.map(removeEnclosingDoubleQuote)
  }

  private def removeEnclosingDoubleQuote(text: String): String = {
    val enclosingText = """"(?s)(.*)"""".r
    text match {
      case enclosingText(str) => str.replace("\"\"", "\"")
      case _ => text
    }
  }

}

object CsvParser extends RegexParsers {
  def apply(src: Source, noHeader: Boolean = false): CsvParser = {
    val lines = src.getLines()
    if (noHeader) {
      new CsvParser(None, src.getLines())
    } else {
      val headerOption = CsvParser.parseAll(record, CsvParser.nextRow(lines))
      val header = headerOption.get
      new CsvParser(Some(header), src.getLines())
    }
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

  private def SEPARATOR = ",".r

  private def field: Parser[String] = s"""(?s)("(""|[^\"])*")|([^\"\r\n$SEPARATOR]*)""".r

  private def fieldList: Parser[Seq[String]] = field ~! rep(SEPARATOR ~> field) ^^ {
    case f ~ l => f +: l
  }

  private def record = fieldList

}