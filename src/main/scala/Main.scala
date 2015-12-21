import java.io.InputStream

import scala.annotation.tailrec
import scala.io._
import scala.util.parsing.combinator._



class CsvParser(lines: Iterator[String]) extends Iterator[List[String]] with RegexParsers {

  private val MAX_LINE_COUNT = 100

  private def SEPARATOR = ",".r

  private def field: Parser[String] = s"""(?s)("(""|[^\"])*")|([^\"\r\n$SEPARATOR]*)""".r

  private def fieldList: Parser[List[String]] = field ~ rep(SEPARATOR ~> field) ^^ {
    case f ~ l => f +: l
  }

  private def record = fieldList

  def next(): List[String] = {
    val row = nextRow(lines)
    val result = parseAll(record, row)
    val fields = result.get
    fields.map(removeEnclosingDoubleQuote)
  }

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

  private def removeEnclosingDoubleQuote(text: String): String = {
    val enclosingText = """"(?s)(.*)"""".r
    text match {
      case enclosingText(str) => str.replace("\"\"", "\"")
      case _ => text
    }
  }

  override def hasNext: Boolean = lines.hasNext
}

object CsvParser {
  def apply(src: Source): CsvParser = new CsvParser(src.getLines())
  def apply(inputStream: InputStream): CsvParser = apply(Source.fromInputStream(inputStream))
}

package object csv4s {
  def main(args: Array[String]) {
    CsvParser(Source.stdin).map(_.mkString("\t")).foreach(println)
  }
}
