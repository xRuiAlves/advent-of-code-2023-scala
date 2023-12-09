//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day9.txt")
      .map(strToIntArr)

    val part1 = input
      .map(findExtrapolatedValue)
      .map(_.last)
      .sum
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def strToIntArr(str: String): Array[Int] = str.split(" ").map(_.toInt)

  def findExtrapolatedValue(sequence: Array[Int]): Array[Int] = if (sequence.forall(_ == 0)) sequence.appended(0) else {
    val diffs = sequence.sliding(2).map(pair => pair.last - pair.head).toArray
    val next = findExtrapolatedValue(diffs)
    sequence.appended(sequence.last + next.last)
  }
}
