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

    def withIncreasedHeatLoss(amount: Int): Node = Node(pos, straightCount, heatLoss + amount)
  }

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day17.txt")
    val map = parseMap(input)

    val part1 = visit(map)
    val part2 = 0

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

  def getNeighbors(map: Mat2D, node: Node): Array[Node] = (node.pos match {
    case NodePos(i, j, 'R') => Array(
      Node(NodePos(i, j + 1, 'R'), node.straightCount + 1, node.heatLoss),
      Node(NodePos(i - 1, j, 'U'), 1, node.heatLoss),
      Node(NodePos(i + 1, j, 'D'), 1, node.heatLoss)
    )
    case NodePos(i, j, 'L') => Array(
      Node(NodePos(i, j - 1, 'L'), node.straightCount + 1, node.heatLoss),
      Node(NodePos(i - 1, j, 'U'), 1, node.heatLoss),
      Node(NodePos(i + 1, j, 'D'), 1, node.heatLoss)
    )
    case NodePos(i, j, 'U') => Array(
      Node(NodePos(i - 1, j, 'U'), node.straightCount + 1, node.heatLoss),
      Node(NodePos(i, j + 1, 'R'), 1, node.heatLoss),
      Node(NodePos(i, j - 1, 'L'), 1, node.heatLoss)
    )
    case NodePos(i, j, 'D') => Array(
      Node(NodePos(i + 1, j, 'D'), node.straightCount + 1, node.heatLoss),
      Node(NodePos(i, j + 1, 'R'), 1, node.heatLoss),
      Node(NodePos(i, j - 1, 'L'), 1, node.heatLoss)
    )
  })
    .filter(node => isInBounds(map, node.pos))
    .filter(node => node.straightCount <= 3)
    .map(node => node.withIncreasedHeatLoss(map(node.pos.i)(node.pos.j)))


  def visit(map: Mat2D): Int = {
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
        getNeighbors(map, curr).foreach(node => toVisit.enqueue(node))
      }
    }

    throw new Error("Path not found!")
  }
}
