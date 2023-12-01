//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

object Day1 {
  def main(args: Array[String]): Unit = {
    val part1 = readResourceLines("day1.txt")
      .map(_.filter(_.isDigit))
      .map(line => line.head.toString + line.last.toString)
      .map(_.toInt)
      .sum

    println(s"Part 1: $part1")
  }
}
