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

    val part1 = maps.map(_.summaryValue).sum
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def parseMap(mapStr: String): Map = {
    val rows = mapStr.split("\n")
    val cols = rows.map(_.toCharArray).transpose.map(_.mkString)
    Map(rows, cols)
  }

  @tailrec
  def hasSymmetry(lines: Array[String], i1: Int, i2: Int): Boolean =
    if (i1 < 0 || i2 >= lines.length) true
    else if (lines(i1) != lines(i2)) false
    else hasSymmetry(lines, i1 - 1, i2 + 1)

  def findSymmetry(lines: Array[String]): Option[Int] = lines
    .indices
    .sliding(2)
    .find { case IndexedSeq(i1, i2) => hasSymmetry(lines, i1, i2) }
    .map(_.last)

  case class Map(rows: Array[String], cols: Array[String]) {
    private val rowSymmetry  = findSymmetry(rows)
    private val colSymmetry = findSymmetry(cols)
    val summaryValue = rowSymmetry.map(_ * 100).getOrElse(0) +  findSymmetry(cols).getOrElse(0)
  }
}
