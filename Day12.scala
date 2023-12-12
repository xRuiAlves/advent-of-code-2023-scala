//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day12 {
  private[this] final val Operational = '.'
  private[this] final val Damaged = '#'
  private[this] final val Unknown = '?'
  private[this] final val Part1UnfoldCount = 1
  private[this] final val Part2UnfoldCount = 5
  private[this] final val CacheKeySeparator = "-"

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day12.txt")
    val cache = mutable.Map[(String, String, Int, Boolean), Long]()

    def count(
        springs: List[Char],
        groups: List[Int],
        numRemainingDamagedSpringsInGroup: Int = 0,
        fillSpringSpace: Boolean = false
    ): Long = {
      val cacheKey = (
        springs.mkString(CacheKeySeparator),
        groups.mkString(CacheKeySeparator),
        numRemainingDamagedSpringsInGroup,
        fillSpringSpace
      )
      cache.get(cacheKey) match {
        case Some(cached) => cached
        case None => {
          val thisCount = (springs, groups, numRemainingDamagedSpringsInGroup, fillSpringSpace) match {
            case (Nil, Nil, 0, _) => 1
            case (Unknown :: springsT, group :: groupsT, 0, false) =>
              count(springsT, groupsT, group - 1, (group - 1) == 0) +
                count(springsT, groups, 0, false)
            case (Unknown :: springsT, Nil, 0, false) => count(springsT, groups, 0, false)
            case (Unknown :: springsT, _, 0, true)    => count(springsT, groups, 0, false)
            case (Operational :: springsT, _, 0, _)   => count(springsT, groups, 0, false)
            case (Damaged :: springsT, group :: groupsT, 0, false) =>
              count(springsT, groupsT, group - 1, (group - 1) == 0)
            case (Unknown :: springsT, _, numRemaining, false) =>
              count(springsT, groups, numRemaining - 1, (numRemaining - 1) == 0)
            case (Damaged :: springsT, _, numRemaining, false) =>
              count(springsT, groups, numRemaining - 1, (numRemaining - 1) == 0)
            case _ => 0L
          }
          cache(cacheKey) = thisCount
          thisCount
        }
      }
    }

    def unfoldRow(row: String, count: Int, token: Char): String = row.appended(token).repeat(count).dropRight(1)

    def countRow(row: String, unfoldCount: Int): Long = count(
      unfoldRow(row.split(" ").head, unfoldCount, '?').toList,
      unfoldRow(row.split(" ").last, unfoldCount, ',').split(",").map(_.toInt).toList
    )

    val part1 = input.map(row => countRow(row, Part1UnfoldCount)).sum
    val part2 = input.map(row => countRow(row, Part2UnfoldCount)).sum

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
