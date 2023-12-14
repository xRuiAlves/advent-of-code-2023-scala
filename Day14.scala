//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable
import scala.annotation.tailrec

object Day14 {
  type Mat2D = Array[Array[Char]]

  private[this] final val RoundRock = 'O';
  private[this] final val EmptySpace = '.';
  private[this] final val Part2NumCycles = 1000000000

  case class CyclicTiltResult(cycleIteration: Int, cycleSize: Int)

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day14.txt")

    val map1 = input.map(_.toCharArray)
    tiltMapNorth(map1)
    val part1 = getLoad(map1)
    println(s"Part 1: $part1")

    val map2 = input.map(_.toCharArray)
    val cycle = tiltUntilCycle(map2)
    val remainingIters = (Part2NumCycles - cycle.cycleIteration) % cycle.cycleSize
    for (_ <- 0 until remainingIters) {
      tiltMap(map2)
    }
    val part2 = getLoad(map2)
    println(s"Part 2: $part2")
  }

  def getLoad(map: Mat2D): Int = map.transpose
    .map(
      _.reverse.zipWithIndex
        .map((cell, i) =>
          if (cell == RoundRock) i + 1
          else 0
        )
        .sum
    )
    .sum

  def tiltUntilCycle(map: Mat2D): CyclicTiltResult = {
    val visited = mutable.Map[String, Int]()

    @tailrec
    def tiltUntilCycle(map: Mat2D, iter: Int): CyclicTiltResult = {
      tiltMap(map)
      val mapHash = map.map(_.mkString).mkString

      visited.get(mapHash) match
        case Some(lastIter) => CyclicTiltResult(iter, iter - lastIter)
        case None => {
          visited(mapHash) = iter
          tiltUntilCycle(map, iter + 1)
        }
    }

    tiltUntilCycle(map, 1)
  }

  def swap(map: Mat2D, row1: Int, col1: Int, row2: Int, col2: Int): Boolean = {
    if (map(row1)(col1) == EmptySpace && map(row2)(col2) == RoundRock) {
      map(row1)(col1) = RoundRock
      map(row2)(col2) = EmptySpace
      true
    } else false
  }

  def tiltMap(map: Mat2D): Unit = {
    tiltMapNorth(map)
    tiltMapWest(map)
    tiltMapSouth(map)
    tiltMapEast(map)
  }

  def tiltMapNorth(map: Mat2D): Unit = map.head.indices.foreach(i => tiltMapNorth(map, i))

  @tailrec
  def tiltMapNorth(map: Mat2D, col: Int): Unit = {
    var swapped = false

    map.indices.sliding(2).foreach { case IndexedSeq(i1, i2) =>
      swapped ||= swap(map, i1, col, i2, col)
    }

    if (swapped) tiltMapNorth(map, col)
  }

  def tiltMapSouth(map: Mat2D): Unit = map.head.indices.foreach(i => tiltMapSouth(map, i))

  @tailrec
  def tiltMapSouth(map: Mat2D, col: Int): Unit = {
    var swapped = false

    map.indices.reverse.sliding(2).foreach { case IndexedSeq(i1, i2) =>
      swapped ||= swap(map, i1, col, i2, col)
    }

    if (swapped) tiltMapSouth(map, col)
  }

  def tiltMapWest(map: Mat2D): Unit = map.indices.foreach(i => tiltMapWest(map, i))

  @tailrec
  def tiltMapWest(map: Mat2D, row: Int): Unit = {
    var swapped = false

    map.head.indices.sliding(2).foreach { case IndexedSeq(i1, i2) =>
      swapped ||= swap(map, row, i1, row, i2)
    }

    if (swapped) tiltMapWest(map, row)
  }

  def tiltMapEast(map: Mat2D): Unit = map.indices.foreach(i => tiltMapEast(map, i))

  @tailrec
  def tiltMapEast(map: Mat2D, row: Int): Unit = {
    var swapped = false

    map.head.indices.reverse.sliding(2).foreach { case IndexedSeq(i1, i2) =>
      swapped ||= swap(map, row, i1, row, i2)
    }

    if (swapped) tiltMapEast(map, row)
  }
}
