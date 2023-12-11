//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day11 {
  type Mat2D = Array[Array[Char]]
  type Coord2D = (Int, Int)

  private[this] final val Galaxy = '#'

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day11.txt")
    val rawUniverse = input.map(_.toCharArray)
    val universe = expandUniverse(rawUniverse)

    val galaxies = (for {
      i <- universe.indices
      j <- universe(i).indices
    } yield {
      if (universe(i)(j) == Galaxy) Some((i, j))
      else None
    }).filter(_.isDefined).map(_.get)

    val part1 = galaxies
      .map(galaxy => bfs(universe, galaxy, galaxies.toSet.filter(_ != galaxy)))
      .sum / 2
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def isEmpty(galaxyLine: Array[Char]): Boolean = !galaxyLine.contains(Galaxy)

  def expandUniverseVertically(universe: Mat2D): Mat2D = universe.flatMap { row =>
    if (isEmpty(row)) Array(row, row.clone())
    else Array(row)
  }

  def expandUniverse(universe: Mat2D): Mat2D = expandUniverseVertically(expandUniverseVertically(universe).transpose).transpose

  case class BfsNode(coord: Coord2D, distance: Int) {
    override def equals(obj: Any): Boolean = coord.equals(obj)
    override def hashCode(): Int = coord.hashCode()
  }

  def isInBounds(universe: Mat2D, coord: Coord2D): Boolean =
    coord._1 >= 0 && coord._2 >= 0 && coord._1 < universe.length && coord._2 < universe(coord._1).length

  def getNeighbors(universe: Mat2D, coord: Coord2D): Array[Coord2D] = Array(
    (coord._1 - 1, coord._2),
    (coord._1 + 1, coord._2),
    (coord._1, coord._2 - 1),
    (coord._1, coord._2 + 1)
  ).filter(coord => isInBounds(universe, coord))

  def bfs(universe: Mat2D, origin: Coord2D, destinations: Set[Coord2D]): Int = {
    var distance = 0
    val visited = mutable.Set[Coord2D]()
    val toVisit = mutable.Queue[BfsNode]()

    toVisit.enqueue(BfsNode(origin, 0))

    while (toVisit.nonEmpty) {
      val curr = toVisit.dequeue()

      if (!visited.contains(curr.coord)) {
        visited.addOne(curr.coord)

        if (destinations.contains(curr.coord)) {
          distance += curr.distance
        }

        toVisit.enqueueAll(getNeighbors(universe, curr.coord).map(neighbor => BfsNode(neighbor, curr.distance + 1)))
      }
    }

    distance
  }
}
