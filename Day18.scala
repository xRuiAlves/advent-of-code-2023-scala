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
    val instructions = parsePaintedInstructions(input)
    val polygonPart1 = Polygon(getPaintedSquares(instructions.map(_.instruction)))

    val part1 = polygonPart1.area
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  case class Polygon(points: Array[Coord2D]) {
    private val shoelaceSum = points.sliding(2).foldLeft(0) {
      case (sum, Array((x1, y1), (x2, y2))) => sum + x1 * y2 - x2 * y1
    }
    val perimeter: Int = points.length

    private val interiorArea = math.abs(shoelaceSum / 2) - perimeter / 2
    val area: Int = interiorArea + perimeter
  }

  case class Instruction(dir: Char, dist: Int)

  case class ColorInstruction(instruction: Instruction, color: String)

  def parsePaintedInstructions(rawInstructions: Array[String]): Array[ColorInstruction] = rawInstructions.map {
    case s"$dir $dist ($color)" => ColorInstruction(Instruction(dir.head, dist.toInt), color)
  }

  def getPaintedSquares(instructions: Array[Instruction]): Array[Coord2D] = {
    val visited = mutable.ArrayBuffer[Coord2D]()
    var curr = (0, 0)
    visited.addOne(curr)

    instructions.foreach { case Instruction(dir, dist) =>
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
