//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day23 {
  type Map = Array[Array[Char]]
  type Coord2D = (Int, Int)

  private[this] final val Path = '.'
  private[this] final val Forest = '#'
  private[this] final val UpwardSlope = '^'
  private[this] final val DownwardSlope = 'v'
  private[this] final val LeftwardSlope = '<'
  private[this] final val RightwardSlope = '>'

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day23.txt")
    val map = input.map(_.toCharArray)
    val start = (0, map.head.indexWhere(_ == Path))

    val part1 = visit(map, start)
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def isInBounds(map: Map, i: Int, j: Int): Boolean =
    i >= 0 && j >= 0 && i < map.length && j < map(i).length

  def getNeighbors(map: Map, coord: Coord2D): Array[Coord2D] = getNeighbors(map, coord._1, coord._2)
  def getNeighbors(map: Map, i: Int, j: Int): Array[Coord2D] = (map(i)(j) match {
    case UpwardSlope    => Array((i - 1, j))
    case DownwardSlope  => Array((i + 1, j))
    case RightwardSlope => Array((i, j + 1))
    case LeftwardSlope  => Array((i, j - 1))
    case _ =>
      Array(
        (i - 1, j),
        (i + 1, j),
        (i, j - 1),
        (i, j + 1)
      )
  })
    .filter(coord => isInBounds(map, coord._1, coord._2))
    .filterNot(coord => map(coord._1)(coord._2) == Forest)

  case class VisitNode(coord: Coord2D, visited: Set[Coord2D])

  def visit(map: Map, start: Coord2D): Int = {
    var largestPath = 0
    val toVisit = mutable.Queue[VisitNode]()
    toVisit.enqueue(VisitNode(start, Set()))

    while (toVisit.nonEmpty) {
      val VisitNode(curr, visited) = toVisit.dequeue()

      if (curr._1 == map.length - 1) {
        largestPath = math.max(largestPath, visited.size)
      } else {
        toVisit.enqueueAll(getNeighbors(map, curr)
          .filterNot(neighbor => visited.contains(neighbor))
          .map(neighbor => VisitNode(neighbor, visited + curr))
        )
      }
    }

    largestPath
  }
}
