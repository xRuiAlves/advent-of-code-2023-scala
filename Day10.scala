//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec

object Day10 {
  final case class Coord2D(x: Int, y: Int) {
    def isAboveOf(other: Coord2D): Boolean = this.y == other.y - 1
    def isBelowOf(other: Coord2D): Boolean = this.y == other.y + 1
    def isLeftOf(other: Coord2D): Boolean = this.x == other.x - 1
    def isRightOf(other: Coord2D): Boolean = this.x == other.x + 1

    lazy val getAbove: Coord2D = Coord2D(x, y - 1)
    lazy val getBelow: Coord2D = Coord2D(x, y + 1)
    lazy val getLeft: Coord2D = Coord2D(x - 1, y)
    lazy val getRight: Coord2D = Coord2D(x + 1, y)
  }

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day10.txt")
    val map = input.map(_.toCharArray)
    val start = {
      val startY = map.indexWhere(_.contains('S'))
      val startX = map(startY).indexOf('S')
      Coord2D(startX, startY)
    }

    @tailrec
    def traverse(curr: Coord2D, prev: Coord2D, distance: Int = 0): Int = {
      if (curr == start && distance > 1) distance
      else {
        traverse(map(curr.y)(curr.x) match {
          case '|' =>
            if (curr.isAboveOf(prev)) curr.getAbove
            else curr.getBelow
          case '-' =>
            if (curr.isLeftOf(prev)) curr.getLeft
            else curr.getRight
          case 'L' =>
            if (curr.isBelowOf(prev)) curr.getRight
            else curr.getAbove
          case 'J' =>
            if (curr.isBelowOf(prev)) curr.getLeft
            else curr.getAbove
          case '7' =>
            if (curr.isAboveOf(prev)) curr.getLeft
            else curr.getBelow
          case 'F' =>
            if (curr.isAboveOf(prev)) curr.getRight
            else curr.getBelow
        }, curr, distance + 1)
      }
    }

    val part1 = traverse(start.getLeft, start, 1) / 2
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
