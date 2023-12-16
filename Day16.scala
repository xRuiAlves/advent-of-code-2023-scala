//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day16 {
  type Mat2D = Array[Array[Char]]
  type Node = ((Int, Int), Char)

  private[this] final val Part1StartPos = ((0, 0), 'R')

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day16.txt")
    val map = input.map(_.toCharArray)

    val part2StartNodes = Array(
      map.indices.map(i => ((i, 0), 'R')),
      map.indices.map(i => ((i, map(i).length - 1), 'L')),
      map.head.indices.map(j => ((0, j), 'D')),
      map.head.indices.map(j => ((map.length - 1, j), 'U'))
    ).flatten

    val part1 = visit(map, Part1StartPos)
    val part2 = part2StartNodes
      .map(startPos => visit(map, startPos))
      .max

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def visit(map: Mat2D, startNode: Node): Int = {
    val visited = mutable.Set[Node]()
    val toVisit = mutable.Queue[Node]()
    toVisit.enqueue(startNode)

    while (toVisit.nonEmpty) {
      val curr = toVisit.dequeue()

      if (!visited.contains(curr) && isInBounds(map, curr._1._1, curr._1._2)) {
        visited.addOne(curr)
        toVisit.enqueueAll(getNeighbors(map, curr))
      }
    }

    visited.map(_._1).size
  }

  def isInBounds(map: Mat2D, i: Int, j: Int): Boolean =
    i >= 0 && j >= 0 && i < map.length && j < map(i).length

  def applyStepForward(node: Node): Node = node match
    case ((i, j), 'R') => ((i, j + 1), 'R')
    case ((i, j), 'L') => ((i, j - 1), 'L')
    case ((i, j), 'U') => ((i - 1, j), 'U')
    case ((i, j), 'D') => ((i + 1, j), 'D')

  def getNeighbors(map: Mat2D, node: Node): Array[Node] = {
    val cell = map(node._1._1)(node._1._2)
    val ((i, j), dir) = node

    if (cell == '.') Array(applyStepForward(node))
    else if (cell == '-') {
      if (dir == 'R' || dir == 'L') Array(applyStepForward(node))
      else
        Array(
          ((i, j - 1), 'L'),
          ((i, j + 1), 'R')
        )
    } else if (cell == '|') {
      if (dir == 'U' || dir == 'D') Array(applyStepForward(node))
      else
        Array(
          ((i - 1, j), 'U'),
          ((i + 1, j), 'D')
        )
    } else if (cell == '\\')
      Array(
        if (dir == 'R') ((i + 1, j), 'D')
        else if (dir == 'L') ((i - 1, j), 'U')
        else if (dir == 'U') ((i, j - 1), 'L')
        else if (dir == 'D') ((i, j + 1), 'R')
        else throw new Error(s"Invalid dir '$dir' at node $node")
      )
    else if (cell == '/')
      Array(
        if (dir == 'R') ((i - 1, j), 'U')
        else if (dir == 'L') ((i + 1, j), 'D')
        else if (dir == 'U') ((i, j + 1), 'R')
        else if (dir == 'D') ((i, j - 1), 'L')
        else throw new Error(s"Invalid dir '$dir' at node $node")
      )
    else throw new Error(s"Invalid cell value '$cell' at node $node")
  }
}
