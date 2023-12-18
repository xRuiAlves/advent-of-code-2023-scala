//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day18 {
  type Coord2D = (Int, Int)
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day18.txt")
    val instructions = parseInstructions(input)
    val paintedSquares = getPaintedSquares(instructions)

    var area = 0
    paintedSquares.sliding(2).foreach { case Array((x1, y1), (x2, y2)) =>
      area += x1 * y2 - x2 * y1
    }
    val perimeter = paintedSquares.length
    val interiorArea = math.abs(area / 2) - perimeter / 2

    val part1 = interiorArea + perimeter
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  case class Instruction(dir: Char, dist: Int, color: String)

  def parseInstructions(rawInstructions: Array[String]): Array[Instruction] = rawInstructions.map {
    case s"$dir $dist ($color)" => Instruction(dir.head, dist.toInt, color)
  }

  def getPaintedSquares(instructions: Array[Instruction]): Array[Coord2D] = {
    val visited = mutable.ArrayBuffer[Coord2D]()
    var curr = (0, 0)
    visited.addOne(curr)

    instructions.foreach { case Instruction(dir, dist, _) =>
      for (_ <- 0 until dist) {
        curr = (curr, dir) match {
          case ((i, j), 'R') => (i, j + 1)
          case ((i, j), 'L') => (i, j - 1)
          case ((i, j), 'U') => (i - 1, j)
          case ((i, j), 'D') => (i + 1, j)
        }
        visited.addOne(curr)
      }
    }

    visited.toArray
  }
}
