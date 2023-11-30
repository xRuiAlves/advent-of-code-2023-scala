package util

import scala.io.Source
import scala.util.Using

object ResourceUtils {
  final def readResourceLines(fileName: String): Array[String] = {
    Using(Source.fromResource(fileName)) {
      _.getLines().toArray
    }.get
  }

  final def readResourceLine(fileName: String): String = {
    readResourceLines(fileName).head
  }
}
