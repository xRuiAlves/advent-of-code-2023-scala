//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines
import scala.annotation.tailrec

object Day6 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day6.txt")

    val times = parseLineNumbers(input.head)
    val distances = parseLineNumbers(input.last)

    val timeTotalRace = parseLineNumber(input.head, "Time: ")
    val distanceTotalRace = parseLineNumber(input.last, "Distance: ")

    val part1 = times
      .zip(distances)
      .map {
        case (time, distanceToBeat) => numWaysToBeatRace(time, distanceToBeat)
      }.product
    val part2 = numWaysToBeatRace(timeTotalRace, distanceTotalRace)

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def parseLineNumbers(line: String): Array[Long] = line.split(" ").filter(_.nonEmpty).drop(1).map(_.toLong)

  def parseLineNumber(line: String, prefix: String): Long = line.stripPrefix(prefix).replace(" ", "").toLong

  @tailrec
  def numWaysToBeatRace(raceTime: Long, distanceToBeat: Long, initialHold: Long = 1L, numWaysAcc: Long = 0L): Long =
    if (initialHold == raceTime) numWaysAcc
    else numWaysToBeatRace(raceTime, distanceToBeat, initialHold + 1, numWaysAcc + (if ((raceTime - initialHold) * initialHold > distanceToBeat) 1L else 0L))
}
