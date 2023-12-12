//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day12 {
  private[this] final val Operational = '.'
  private[this] final val Damaged = '#'
  private[this] final val Unknown = '?'

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day12.txt")

    val part1 = input
      .map(row => count(
        row.split(" ").head.toList,
        row.split(" ").last.split(",").map(_.toInt).toList
      )).sum
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def count(
    springs: List[Char],
    groups: List[Int],
    numRemainingDamagedSpringsInGroup: Int = 0,
    fillSpringSpace: Boolean = false
  ): Long = (springs, groups, numRemainingDamagedSpringsInGroup, fillSpringSpace) match {
    case (Nil, Nil, 0, _) => 1
    case (Unknown :: springsT, group :: groupsT, 0, false) => (
      count(springsT, groupsT, group - 1, (group - 1) == 0) +
      count(springsT, groups, 0, false)
    )
    case (Unknown :: springsT, Nil, 0, false) => count(springsT, groups, 0, false)
    case (Unknown :: springsT, _, 0, true) => count(springsT, groups, 0, false)
    case (Operational :: springsT, _, 0, _) => count(springsT, groups, 0, false)
    case (Damaged :: springsT,  group :: groupsT, 0, false) => count(springsT, groupsT, group - 1, (group - 1) == 0)
    case (Unknown :: springsT, _, numRemaining, false) => count(springsT, groups, numRemaining - 1, (numRemaining - 1) == 0)
    case (Damaged :: springsT, _, numRemaining, false) => count(springsT, groups, numRemaining - 1, (numRemaining - 1) == 0)
    case _ => 0L
  }
}
