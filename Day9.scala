//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

object Day9 {
  def main(args: Array[String]): Unit = {
    val extrapolatedValues = readResourceLines("day9.txt")
      .map(_.split(" ").map(_.toInt))
      .map(findExtrapolatedValue)

    val part1 = extrapolatedValues.map(_.last).sum
    val part2 = extrapolatedValues.map(_.head).sum

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def findExtrapolatedValue(sequence: Array[Int]): Array[Int] =
    if (sequence.forall(_ == 0)) 0 +: sequence :+ 0
    else {
      val diffs = sequence.sliding(2).map(pair => pair.last - pair.head).toArray
      val next = findExtrapolatedValue(diffs)
      (sequence.head - next.head) +: sequence :+ (sequence.last + next.last)
    }
}
