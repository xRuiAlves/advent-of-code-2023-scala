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

  case class BfsNode(coord: Coord2D, distance: Int)

  case class Distance(from: Coord2D, to: Coord2D, distance: Int) {
    private lazy val minRow = math.min(from._1, to._1)
    private lazy val maxRow = math.max(from._1, to._1)
    private lazy val minCol = math.min(from._2, to._2)
    private lazy val maxCol = math.max(from._2, to._2)

    def expandedValue(emptyRows: Array[Int], emptyCols: Array[Int], expansionFactor: Int): Long = {
      val emptyRowDiff = emptyRows.count(row => row > minRow && row < maxRow).toLong
      val emptyColDiff = emptyCols.count(col => col > minCol && col < maxCol).toLong
      distance + expansionFactor * (emptyRowDiff + emptyColDiff)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day11.txt")
    val universe = input.map(_.toCharArray)
    val emptyRows = getEmptyLineIndices(universe)
    val emptyCols = getEmptyLineIndices(universe.transpose)

    val galaxies = (for {
      i <- universe.indices
      j <- universe(i).indices
    } yield {
      if (universe(i)(j) == Galaxy) Some((i, j))
      else None
    }).filter(_.isDefined).map(_.get)

    val distances = galaxies.map(galaxy => bfs(universe, galaxy, galaxies.toSet.filter(_ != galaxy))).toArray

    val part1 = getExpandedDistancesValue(distances, emptyRows, emptyCols, 1)
    val part2 = getExpandedDistancesValue(distances, emptyRows, emptyCols, 1000000 - 1)

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def getExpandedDistancesValue(
      distances: Array[Array[Distance]],
      emptyRows: Array[Int],
      emptyCols: Array[Int],
      expansionFactor: Int
  ): Long = distances
    .map(_.map(distance => distance.expandedValue(emptyRows, emptyCols, expansionFactor)).sum)
    .sum / 2

  def getEmptyLineIndices(galaxy: Mat2D): Array[Int] = galaxy.zipWithIndex
    .filter(line => !line._1.contains(Galaxy))
    .map(_._2)

  def isInBounds(universe: Mat2D, coord: Coord2D): Boolean =
    coord._1 >= 0 && coord._2 >= 0 && coord._1 < universe.length && coord._2 < universe(coord._1).length

  def getNeighbors(universe: Mat2D, coord: Coord2D): Array[Coord2D] = Array(
    (coord._1 - 1, coord._2),
    (coord._1 + 1, coord._2),
    (coord._1, coord._2 - 1),
    (coord._1, coord._2 + 1)
  ).filter(coord => isInBounds(universe, coord))

  def bfs(universe: Mat2D, origin: Coord2D, destinations: Set[Coord2D]): Array[Distance] = {
    val distances = mutable.ArrayBuffer[Distance]()
    val visited = mutable.Set[Coord2D]()
    val toVisit = mutable.Queue[BfsNode]()

    toVisit.enqueue(BfsNode(origin, 0))

    while (toVisit.nonEmpty) {
      val curr = toVisit.dequeue()

      if (!visited.contains(curr.coord)) {
        visited.addOne(curr.coord)

        if (destinations.contains(curr.coord)) {
          distances.addOne(Distance(origin, curr.coord, curr.distance))
        }

        toVisit.enqueueAll(getNeighbors(universe, curr.coord).map(neighbor => BfsNode(neighbor, curr.distance + 1)))
      }
    }

    distances.toArray
  }
}
