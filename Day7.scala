//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

object Day7 {

  class Hand(private val handStr: String) extends Ordered[Hand] {
    private val cardsStr = handStr.split(" ").head
    protected val cards: Array[Char] = cardsStr.toCharArray
    protected val cardCounts: Map[Char, Int] = cards.groupMapReduce(identity)(_ => 1)(_ + _)

    val value: Int = handStr.split(" ").last.toInt

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

    protected val cardsOrdering: Array[Char] = Array('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
    override def compare(that: Hand): Int = {
      if (this.handType != that.handType) this.handType - that.handType
      else
        this.cards
          .zip(that.cards)
          .dropWhile((thisCard, thatCard) => thisCard == thatCard)
          .head match {
          case (thisCard, thatCard) => cardsOrdering.indexOf(thisCard) - cardsOrdering.indexOf(thatCard)
        }
    }
  }

  case class JokerHand(private val jokerHandStr: String) extends Hand(jokerHandStr) {
    private val numJokers = cards.count(_ == 'J')
    private val cardCountsExceptJokers = cardCounts.filter(_._1 != 'J')
    override protected val cardsOrdering: Array[Char] =
      Array('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')

    override val handType: Int = {
      if (cardCounts.size == 1) 7 // 5 of a kind
      else if (cardCountsExceptJokers.values.exists(_ == 4))
        if (numJokers == 1) 7 // 5 of a kind
        else 6 // 4 of a kind
      else if (cardCountsExceptJokers.values.exists(_ == 3))
        if (numJokers == 2) 7 // 5 of a kind
        else if (numJokers == 1) 6 // 4 of a kind
        else if (cardCountsExceptJokers.values.exists(_ == 2)) 5 // Full House
        else 4 // trio
      else if (cardCountsExceptJokers.values.count(_ == 2) == 2)
        if (numJokers == 1) 5 // Full House
        else 3 // 2 pairs
      else if (cardCountsExceptJokers.values.count(_ == 2) == 1)
        if (numJokers == 3) 7 // 5 of a kind
        else if (numJokers == 2) 6 // 4 of a kind
        else if (numJokers == 1) 4 // trio
        else 2 // pair
      else if (numJokers == 4) 7 // 5 of a kind
      else if (numJokers == 3) 6 // 4 of a kind
      else if (numJokers == 2) 4 // trio
      else if (numJokers == 1) 2 // pair
      else 1 // single card
    }
  }

  def getWinnings(hands: Array[Hand]): Int = hands.sorted.zipWithIndex.map { case (hand, rank) =>
    (rank + 1) * hand.value
  }.sum

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day7.txt")

    val part1 = getWinnings(input.map(line => new Hand(line)))
    val part2 = getWinnings(input.map(line => JokerHand(line)))

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
