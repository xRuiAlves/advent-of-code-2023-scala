//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

object Day4 {

  final case class Card(private val cardStr: String) {
    final val numMatchingNumbers = {
      val numberGroups = cardStr.split(": ").last.split(" \\| ")
      val winningNumbers = Card.parseNumbersStr(numberGroups.head)
      val myNumbers = Card.parseNumbersStr(numberGroups.last)
      myNumbers.count(num => winningNumbers.contains(num))
    }

    final val value =
      if (numMatchingNumbers == 0) 0
      else 1 << (numMatchingNumbers - 1)
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
    val cardCounts = cards.map(_ => 0)

    def visit(cardId: Int): Unit = if (cardId < cards.length) {
      cardCounts(cardId) += 1
      for (i <- 0 until cards(cardId).numMatchingNumbers) visit(cardId + i + 1)
    }

    cards.indices.foreach(visit)

    val part1 = cards.map(_.value).sum
    val part2 = cardCounts.sum

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
