//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec

object Day13 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day13.txt")
    val matrices = input.mkString("\n").split("\n\n")
    val maps = matrices.map(parseMap)

    val part1 = maps.map(_.getSummaryValue(0)).sum
    val part2 = maps.map(_.getSummaryValue(1)).sum

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def parseMap(mapStr: String): Map = {
    val rows = mapStr.split("\n")
    val cols = rows.map(_.toCharArray).transpose.map(_.mkString)
    Map(rows, cols)
  }

  def countDifferences(line1: String, line2: String): Int = line1.indices.count(i => line1(i) != line2(i))

  @tailrec
  def countSmudges(lines: Array[String], i1: Int, i2: Int, smudges: Int = 0): Int =
    if (i1 < 0 || i2 >= lines.length) smudges
    else countSmudges(lines, i1 - 1, i2 + 1, smudges + countDifferences(lines(i1), lines(i2)))

  def findSymmetry(lines: Array[String], smudgeCount: Int): Option[Int] = lines
    .indices
    .sliding(2)
    .find { case IndexedSeq(i1, i2) => countSmudges(lines, i1, i2) == smudgeCount }
    .map(_.last)

  case class Map(rows: Array[String], cols: Array[String]) {
    def getSummaryValue(smudgeCount: Int): Int = (
      findSymmetry(rows, smudgeCount).map(_ * 100).getOrElse(0) +
      findSymmetry(cols, smudgeCount).getOrElse(0)
    )
  }
}
