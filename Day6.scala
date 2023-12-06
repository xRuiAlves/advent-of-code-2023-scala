//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day6 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day6.txt")

    val times = input.head.split(" ").filter(_.nonEmpty).drop(1).map(_.toInt)
    val distances = input.last.split(" ").filter(_.nonEmpty).drop(1).map(_.toInt)

    val part1 = times.zip(distances).map {
      case (time, distanceToBeat) => numWaysToBeatRace(time, distanceToBeat)
    }.product
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  @tailrec
  def numWaysToBeatRace(raceTime: Int, distanceToBeat: Int, initialHold: Int = 1, numWaysAcc: Int = 0): Int = if (initialHold == raceTime) numWaysAcc else {
    val myDistance = (raceTime - initialHold) * initialHold
    numWaysToBeatRace(raceTime, distanceToBeat, initialHold + 1, numWaysAcc + (if (myDistance > distanceToBeat) 1 else 0))
  }
}
