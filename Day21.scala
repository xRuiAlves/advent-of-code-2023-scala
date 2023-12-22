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
  private[this] final val NumStepsPart1 = 64
  private[this] final val NumStepsPart2 = 26501365

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day21.txt")
    val map = input.map(_.toCharArray)
    val gardens = getGardens(map)
    val start = (map.length / 2, map.length / 2)

    val interpolationIndexes = getInterpolationIndexes(map.length)
    val visited = visit(gardens, map.length, start, interpolationIndexes.last)

    val part1 = visited.count(_.depth == NumStepsPart1)
    val part2 = interpolation(visited, map.length, interpolationIndexes, NumStepsPart2)

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def getGardens(map: Map): Set[Coord2D] = {
    val gardens = mutable.Set[Coord2D]()

    for (i <- map.indices; j <- map(i).indices) {
      if (map(i)(j) == Start) {
        map(i)(j) = Garden
      }
      if (map(i)(j) == Garden) {
        gardens.addOne((i, j))
      }
    }

    gardens.toSet
  }

  def getGardenNeighbors(gardens: Set[Coord2D], mapSize: Int, coord: Coord2D): Set[Coord2D] =
    getGardenNeighbors(gardens, mapSize, coord._1, coord._2)
  def getGardenNeighbors(gardens: Set[Coord2D], mapSize: Int, i: Int, j: Int): Set[Coord2D] = Set(
    (i, j - 1),
    (i, j + 1),
    (i - 1, j),
    (i + 1, j)
  ).filter { case (ni, nj) =>
    gardens.contains((Math.floorMod(ni, mapSize), Math.floorMod(nj, mapSize)))
  }

  case class VisitNode(coord: Coord2D, depth: Int)

  def visit(gardens: Set[Coord2D], mapSize: Int, start: Coord2D, targetDepth: Int): Set[VisitNode] = {
    val toVisit = mutable.Queue[VisitNode]()
    val visited = mutable.Set[VisitNode]()
    toVisit.enqueue(VisitNode(start, 0))

    while (toVisit.nonEmpty) {
      val curr = toVisit.dequeue()

      if (!visited.contains(curr) && curr.depth <= targetDepth) {
        visited.addOne(curr)
        getGardenNeighbors(gardens, mapSize, curr.coord).foreach(neighbor => toVisit.enqueue(VisitNode(neighbor, curr.depth + 1)))
      }
    }

    visited.toSet
  }

  def getInterpolationIndexes(mapSize: Int): IndexedSeq[Int] = for (i <- 0 to 2) yield mapSize / 2 + mapSize * i

  // https://en.wikipedia.org/wiki/Polynomial_interpolation
  // I'm a bit at loss at why this works, but after way too many off-by-(a few) errors, I managed to get the right value
  // Got some inspiration from Reddit in regards to the use of the polynomial interplation, but I still think there's something
  // off with my formula, although I can't put my finger on what it might be 🤷
  def interpolation(visited: Set[VisitNode], mapSize: Int, interpolationIndexes: IndexedSeq[Int], numSteps: Int): Long = {
    val interpolationPoints = interpolationIndexes.map(i => visited.count(_.depth == i).toLong)

    val a = interpolationPoints(0)
    val b = interpolationPoints(1) - interpolationPoints(0)
    val c = interpolationPoints(2) - interpolationPoints(1)
    val numInterpolationExpansions = numSteps / mapSize

    a + b * numInterpolationExpansions + (numInterpolationExpansions * (numInterpolationExpansions - 1L) / 2L) * (c - b)
  }
}
