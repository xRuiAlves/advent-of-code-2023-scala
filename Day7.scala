//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec

object Day7 {
  private[this] final val CardsOrderingPart1 = Array('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
  private[this] final val CardsOrderingPart2 = Array('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')

  final case class Hand(private val cardsStr: String, value: Int) {
    val cards: Array[Char] = cardsStr.toCharArray
    private val cardCounts = cards.groupMapReduce(identity)(_ => 1)(_ + _)

    val handTypePart1: Int = {
      if (cardCounts.size == 1) 7
      else if (cardCounts.values.exists(_ == 4)) 6
      else if (cardCounts.values.exists(_ == 3))
        if (cardCounts.values.exists(_ == 2)) 5
        else 4
      else if (cardCounts.values.count(_ == 2) == 2) 3
      else if (cardCounts.values.count(_ == 2) == 1) 2
      else 1
    }

    private val numJokers = cards.count(_ == 'J')
    private val cardCountsExceptJokers = cardCounts.filter(_._1 != 'J')

    val handTypePart2: Int = {
      if (cardCounts.size == 1) 7 // 5 of a kind
      else if (cardCountsExceptJokers.values.exists(_ == 4))
        if (numJokers == 1) 7 // 5 of a kind
        else 6  // 4 of a kind
      else if (cardCountsExceptJokers.values.exists(_ == 3))
        if (numJokers == 2) 7 // 5 of a kind
        else if (numJokers == 1) 6  // 4 of a kind
        else if (cardCountsExceptJokers.values.exists(_ == 2)) 5  // Full House
        else 4  // trio
      else if (cardCountsExceptJokers.values.count(_ == 2) == 2)
        if (numJokers == 1) 5 // Full House
        else 3 // 2 pairs
      else if (cardCountsExceptJokers.values.count(_ == 2) == 1)
        if (numJokers == 3) 7 // 5 of a kind
        else if (numJokers == 2) 6 // 4 of a kind
        else if (numJokers == 1) 4 // trio
        else 2 // pair
      else
        if (numJokers == 4) 7 // 5 of a kind
        else if (numJokers == 3) 6 // 4 of a kind
        else if (numJokers == 2) 4 // trio
        else if (numJokers == 1) 2 // pair
        else 1 // single card
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

    val part1 = hands
      .sortWith(compareHands)
      .zipWithIndex
      .map { case (hand, rank) => (rank + 1) * hand.value }
      .sum
    val part2 = hands
      .sortWith(compareHandsPart2)
      .zipWithIndex
      .map { case (hand, rank) => (rank + 1) * hand.value }
      .sum

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def compareHands(hand1: Hand, hand2: Hand): Boolean = {
    if (hand1.handTypePart1 != hand2.handTypePart1) hand1.handTypePart1 < hand2.handTypePart1
    else
      hand1.cards
        .zip(hand2.cards)
        .dropWhile((hand1card, hand2card) => hand1card == hand2card)
        .head match {
        case (hand1card, hand2card) => CardsOrderingPart1.indexOf(hand1card) < CardsOrderingPart1.indexOf(hand2card)
      }
  }

  def compareHandsPart2(hand1: Hand, hand2: Hand): Boolean = {
    if (hand1.handTypePart2 != hand2.handTypePart2) hand1.handTypePart2 < hand2.handTypePart2
    else
      hand1.cards
        .zip(hand2.cards)
        .dropWhile((hand1card, hand2card) => hand1card == hand2card)
        .head match {
        case (hand1card, hand2card) => CardsOrderingPart2.indexOf(hand1card) < CardsOrderingPart2.indexOf(hand2card)
      }
  }
}
