//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec

object Day8 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day8.txt")
    val instructions = input
      .head
      .toCharArray
      .map {
        case 'L' => 0
        case 'R' => 1
      }
    val map = input
      .drop(2)
      .map(line => {
        val from = line.split(" = ").head
        val to = line.split(" = ").last.split(", ").map(_.filter(_.isLetter))
        from -> to
      })
      .toMap

    @tailrec
    def findDestination(curr: String, steps: Int = 0): Int =
      if (curr == "ZZZ") steps
      else findDestination(map(curr)(instructions(steps % instructions.length)), steps + 1)

    val part1 = findDestination("AAA")
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

}
