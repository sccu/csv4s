package name.sccu.csv4s

import scala.io.Source

object Main {
  def main(args: Array[String]) {
    CsvParser(Source.stdin).map(_.mkString("\t")).foreach(println)
  }
}
