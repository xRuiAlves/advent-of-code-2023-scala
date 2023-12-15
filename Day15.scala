//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLine

import scala.annotation.tailrec
import scala.collection.mutable

object Day15 {
  private[this] final val ASCII_MOD_SPACE = 256
  private[this] final val ASCII_MULTIPLIER = 17

  def main(args: Array[String]): Unit = {
    val input = readResourceLine("day15.txt")
    val words = input.split(",")

    val part1 = words.map(getHash).sum
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def getHash(word: String): Int = word
    .toCharArray
    .foldLeft(0)((acc, curr) => (acc + curr) * ASCII_MULTIPLIER % ASCII_MOD_SPACE)
}
