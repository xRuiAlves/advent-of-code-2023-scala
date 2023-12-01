//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import scala.collection.mutable
import util.ResourceUtils.readResourceLines

object Day1 {
  private[this] final val StringDigits = Map(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9'
  )

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day1.txt")

    val part1 = amend(input)
    val part2 = amend(input.map(getLineDigits))

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def amend(lines: Array[String]): Int = lines
    .map(_.filter(_.isDigit))
    .map(line => line.head.toString + line.last.toString)
    .map(_.toInt)
    .sum

  def getLineDigits(line: String): String = {
    val digits = mutable.ArrayBuffer[Char]()

    for (i <- 0 until line.length) {
      if (line.charAt(i).isDigit) digits.addOne(line.charAt(i))
      else {
        val subLine = line.substring(i)
        StringDigits.keys.map(num => if (subLine.startsWith(num)) digits.addOne(StringDigits(num)))
      }
    }

    digits.mkString
  }
}
