//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec

object Day7 {
  private[this] final val Cards = Array('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')

  final case class Hand(cardsStr: String, value: Int) {
    val cards: Array[Char] = cardsStr.toCharArray
    val cardCounts: Map[Char, Int] = cards.groupMapReduce(identity)(_ => 1)(_ + _)

    val handType: Int = {
      if (cardCounts.size == 1) 7
      else if (cardCounts.values.exists(_ == 4)) 6
      else if (cardCounts.values.exists(_ == 3))
        if (cardCounts.values.exists(_ == 2)) 5
        else 4
      else if (cardCounts.values.count(_ == 2) == 2) 3
      else if (cardCounts.values.count(_ == 2) == 1) 2
      else 1
    }
  }

  object Hand {
    def fromStr(str: String): Hand = str.split(" ") match
      case Array(cardsStr, value) => Hand(cardsStr, value.toInt)
  }

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day7.txt")

    val hands = input
      .map(Hand.fromStr)
      .sortWith(compareHands)

    val part1 = hands.zipWithIndex.map { case (hand, rank) =>
      (rank + 1) * hand.value
    }.sum
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def compareHands(hand1: Hand, hand2: Hand): Boolean = {
    if (hand1.handType != hand2.handType) hand1.handType < hand2.handType
    else
      hand1.cards
        .zip(hand2.cards)
        .dropWhile((hand1card, hand2card) => hand1card == hand2card)
        .head match {
        case (hand1card, hand2card) => Cards.indexOf(hand1card) < Cards.indexOf(hand2card)
      }
  }
}
