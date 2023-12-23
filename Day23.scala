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
  type GraphEdge = (Coord2D, Int)
  type Graph = mutable.Map[Coord2D, Array[GraphEdge]]

  private[this] final val Path = '.'
  private[this] final val Forest = '#'
  private[this] final val UpwardSlope = '^'
  private[this] final val DownwardSlope = 'v'
  private[this] final val LeftwardSlope = '<'
  private[this] final val RightwardSlope = '>'

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day23.txt")
    val map = input.map(_.toCharArray)
    val graph = buildGraph(map)
    val start = (0, map.head.indexWhere(_ == Path))
    val end = (map.length - 1, map.last.indexWhere(_ == Path))

    val part1 = visit(graph, start, end)
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def buildGraph(map: Map): Graph = {
    val graph = mutable.Map[Coord2D, Array[GraphEdge]]()
    for (i <- map.indices; j <- map(i).indices) if (isInBounds(map, i, j) && map(i)(j) != Forest) {
      graph((i, j)) = getNeighbors(map, i, j).map(coord => (coord, 1))
    }
    graph
  }

  def isInBounds(map: Map, i: Int, j: Int): Boolean =
    i >= 0 && j >= 0 && i < map.length && j < map(i).length

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

  case class VisitNode(coord: Coord2D, dist: Int, visited: Set[Coord2D])

  def visit(graph: Graph, start: Coord2D, end: Coord2D): Int = {
    var largestPath = 0
    val toVisit = mutable.Queue[VisitNode]()
    toVisit.enqueue(VisitNode(start, 0, Set()))

    while (toVisit.nonEmpty) {
      val VisitNode(curr, dist, visited) = toVisit.dequeue()

      if (curr == end) {
        largestPath = math.max(largestPath, dist)
      } else {
        toVisit.enqueueAll(graph(curr)
          .filterNot(graphEdge => visited.contains(graphEdge._1))
          .map(graphEdge => VisitNode(graphEdge._1, dist + graphEdge._2, visited + curr))
        )
      }
    }

    largestPath
  }
}
