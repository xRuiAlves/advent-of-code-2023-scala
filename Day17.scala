//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day17 {
  type Mat2D = Array[Array[Int]]

  case class NodePos(i: Int, j: Int, dir: Char)

  case class Node(pos: NodePos, straightCount: Int, heatLoss: Int) extends Ordered[Node] {
    def compare(that: Node) = that.heatLoss.compareTo(this.heatLoss)
  }

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day17.txt")
    val map = parseMap(input)

    val part1 = visitPart1(map)
    val part2 = visitPart2(map)

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def parseMap(rawMap: Array[String]): Mat2D = rawMap.map(_.map(digit => digit.toInt - '0').toArray)

  def isInBounds(map: Mat2D, pos: NodePos): Boolean = pos match { case NodePos(i, j, _) =>
    i >= 0 && j >= 0 && i < map.length && j < map(i).length
  }

  def isTargetNode(map: Mat2D, pos: NodePos): Boolean = pos match { case NodePos(i, j, _) =>
    (i == map.length - 1) && (j == map(i).length - 1)
  }

  def getForwardPos(pos: NodePos): NodePos = pos match {
    case NodePos(i, j, 'R') => NodePos(i, j + 1, 'R')
    case NodePos(i, j, 'L') => NodePos(i, j - 1, 'L')
    case NodePos(i, j, 'U') => NodePos(i - 1, j, 'U')
    case NodePos(i, j, 'D') => NodePos(i + 1, j, 'D')
  }

  def getAfterTurningPositions(pos: NodePos): Array[NodePos] = (pos match {
    case NodePos(i, j, 'R') => Array(NodePos(i, j, 'U'), NodePos(i, j, 'D'))
    case NodePos(i, j, 'L') => Array(NodePos(i, j, 'U'), NodePos(i, j, 'D'))
    case NodePos(i, j, 'U') => Array(NodePos(i, j, 'L'), NodePos(i, j, 'R'))
    case NodePos(i, j, 'D') => Array(NodePos(i, j, 'L'), NodePos(i, j, 'R'))
  }).map(getForwardPos)

  def getNeighborsPart1(map: Mat2D, node: Node): mutable.ArrayBuffer[Node] = {
    val neighbors = mutable.ArrayBuffer[Node]()

    if (node.straightCount < 3) {
      val newPos = getForwardPos(node.pos)
      if (isInBounds(map, newPos)) {
        neighbors.addOne(Node(newPos, node.straightCount + 1, node.heatLoss + map(newPos.i)(newPos.j)))
      }
    }

    getAfterTurningPositions(node.pos)
      .filter(newPos => isInBounds(map, newPos))
      .foreach(newPos => {
        neighbors.addOne(Node(newPos, 1, node.heatLoss + map(newPos.i)(newPos.j)))
      })

    neighbors
  }

  def visitPart1(map: Mat2D): Int = {
    val visited = mutable.Set[(NodePos, Int)]()
    val toVisit = mutable.PriorityQueue[Node]()
    val startNode = Node(NodePos(0, 0, 'R'), 0, 0)
    toVisit.enqueue(startNode)

    while (toVisit.nonEmpty) {
      val curr = toVisit.dequeue()
      val visitedHash = (curr.pos, curr.straightCount)

      if (!visited.contains(visitedHash)) {

        if (isTargetNode(map, curr.pos)) {
          return curr.heatLoss
        }

        visited.addOne(visitedHash)
        getNeighborsPart1(map, curr).foreach(node => toVisit.enqueue(node))
      }
    }

    throw new Error("Path not found!")
  }
}
