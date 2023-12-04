//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import Day4.Card.parseNumbersStr
import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day4 {

  final case class Card(private val cardStr: String) {
    final val value = {
      val numberGroups = cardStr.split(": ").last.split(" \\| ")
      val winningNumbers = parseNumbersStr(numberGroups.head)
      val myNumbers = parseNumbersStr(numberGroups.last)
      val myWinningNumbers = myNumbers.count(num => winningNumbers.contains(num))

      if (myWinningNumbers == 0) 0
      else 1 << (myWinningNumbers - 1)
    }
  }
  object Card {
    private final def parseNumbersStr(numbersStr: String): Set[Int] = numbersStr
      .split(" ")
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toSet
  }

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day4.txt")
    val cards = input.map(cardStr => Card(cardStr))

    val part1 = cards.map(_.value).sum
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
