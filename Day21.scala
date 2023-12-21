//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using file util/MathUtils.scala
//> using resourceDir inputs

import util.MathUtils.lcm
import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day21 {
  type Map = Array[Array[Char]]
  type Coord2D = (Int, Int)

  private[this] final val Start = 'S'
  private[this] final val Garden = '.'
  private[this] final val Rock = '#'
  private[this] final val NumSteps = 64

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day21.txt")
    val map = input.map(_.toCharArray)
    val start = getMapStart(map)

    val part1 = twoStepVisit(map, start, NumSteps)
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def getMapStart(map: Map): Coord2D = {
    for (i <- map.indices; j <- map(i).indices) if (map(i)(j) == Start) {
      map(i)(j) = Garden
      return (i, j)
    }
    throw new Error("Map start position not found!")
  }

  def isInBounds(map: Map, i: Int, j: Int): Boolean =
    i >= 0 && j >= 0 && i < map.length && j < map(i).length

  def getGardenNeighbors(map: Map, coord: Coord2D): Set[Coord2D] =
    getGardenNeighbors(map, coord._1, coord._2)
  def getGardenNeighbors(map: Map, i: Int, j: Int): Set[Coord2D] = Set(
    (i, j - 1),
    (i, j + 1),
    (i - 1, j),
    (i + 1, j)
  ).filter {
    case (ni, nj) => isInBounds(map, ni, nj) && map(ni)(nj) == Garden
  }

  def getGardenTwoStepNeighbors(map: Map, coord: Coord2D): Set[Coord2D] =
    getGardenNeighbors(map, coord).flatMap(neighbor => getGardenNeighbors(map, neighbor))

  case class SearchNode(coord: Coord2D, depth: Int)

  def twoStepVisit(map: Map, start: Coord2D, targetDepth: Int): Int = {
    val visited = mutable.Set[Coord2D]()
    val toVisit = mutable.Queue[SearchNode]()
    toVisit.enqueue(SearchNode(start, 0))

    while (toVisit.nonEmpty) {
      val SearchNode(coord, depth) = toVisit.dequeue()

      if (!visited.contains(coord) && depth <= targetDepth) {
        visited.addOne(coord)
        getGardenTwoStepNeighbors(map, coord).foreach(neighbor => toVisit.enqueue(SearchNode(neighbor, depth + 2)))
      }
    }

    visited.size
  }

}
