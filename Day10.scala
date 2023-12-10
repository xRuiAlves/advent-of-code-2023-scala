//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable
import scala.annotation.tailrec

object Day10 {
  private[this] final val TopConnectedVerticalPipes = Array('|', 'L', 'J')

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
    val path = mutable.Set[Coord2D]()
    val startY = map.indexWhere(_.contains('S'))
    val startX = map(startY).indexOf('S')
    val start = Coord2D(startX, startY)

    val startIsLeftConnected = map(startY).indices.contains(startX - 1) && Array('-', 'L', 'F').contains(map(startY)(startX - 1))
    val startIsRightConnected = map(startY).indices.contains(startX + 1) && Array('-', '7', 'J').contains(map(startY)(startX + 1))
    val startIsAboveConnected = map.indices.contains(startY - 1) && Array('|', '7', 'F').contains(map(startY - 1)(startX))
    val startIsBelowConnected = map.indices.contains(startY + 1) && Array('|', 'J', 'L').contains(map(startY + 1)(startX))

    map(startY)(startX) =
      if (startIsLeftConnected && startIsAboveConnected) 'J'
      else if (startIsLeftConnected && startIsBelowConnected) '7'
      else if (startIsRightConnected && startIsAboveConnected) 'L'
      else if (startIsRightConnected && startIsBelowConnected) 'F'
      else if (startIsLeftConnected && startIsRightConnected) '-'
      else if (startIsAboveConnected && startIsBelowConnected) '|'
      else throw new Error("Invalid start!")

    @tailrec
    def traverse(curr: Coord2D, prev: Coord2D, distance: Int): Int = {
      path.addOne(curr)
      {
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
    }

    @tailrec
    def countTopConnectedVerticalPipesOnTheLeft(coord: Coord2D, count: Int = 0): Int = {
      if (coord.x < 0) count
      else countTopConnectedVerticalPipesOnTheLeft(
        coord.getLeft,
        count + TopConnectedVerticalPipes.count(_ == map(coord.y)(coord.x))
      )
    }

    def isInBounds(coord: Coord2D): Boolean = countTopConnectedVerticalPipesOnTheLeft(coord) % 2 == 1

    def cleanupMap(): Unit = {
      for (y <- map.indices; x <- map(y).indices) {
        if (!path.contains(Coord2D(x, y))) {
          map(y)(x) = '.'
        }
      }
    }

    val part1 = traverse(
      if (startIsLeftConnected) start.getLeft
      else if (startIsRightConnected) start.getRight
      else if (startIsBelowConnected) start.getBelow
      else start.getAbove,
      start,
      1
    ) / 2

    cleanupMap()
    
    val part2 = (for (y <- map.indices; x <- map(y).indices) yield Coord2D(x, y))
      .filterNot(path.contains)
      .count(isInBounds)

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
