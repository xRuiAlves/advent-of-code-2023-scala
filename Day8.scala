//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using file util/MathUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines
import util.MathUtils.lcm

import scala.annotation.tailrec

object Day8 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day8.txt")

    val instructions = input
      .head
      .toCharArray
      .map(instruction => "LR".indexOf(instruction))

    val map = input
      .drop(2)
      .map(line => {
        val from = line.split(" = ").head
        val to = line.split(" = ").last.split(", ").map(_.filter(_.isLetterOrDigit))
        from -> to
      })
      .toMap

    @tailrec
    def findDestination(curr: String, destinationSuffix: String, steps: Int = 0): Int =
      if (curr.endsWith(destinationSuffix)) steps
      else findDestination(map(curr)(instructions(steps % instructions.length)), destinationSuffix, steps + 1)

    val part1 = findDestination("AAA", "ZZZ")
    val part2 = lcm(map
      .keys
      .filter(_.last == 'A')
      .map(node => findDestination(node, "Z"))
      .map(_.toLong)
      .toArray
    )

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
