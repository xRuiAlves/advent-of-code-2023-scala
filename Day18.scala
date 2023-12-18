//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day18 {
  type Coord2D = (Long, Long)
  type Line2D = (Coord2D, Coord2D)

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day18.txt")
    val coloredInstructions = parseColoredInstructions(input)

    lazy val part1 = getPolygonAreaFromInstructions(coloredInstructions.map(_.instruction))
    lazy val part2 = getPolygonAreaFromInstructions(parseInstructionsFromColors(coloredInstructions.map(_.color)))

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  case class Polygon(lines: Array[Line2D]) {
    private val shoelaceSum = lines
      .map(getPointsFromLine)
      .map(points => points.sliding(2).foldLeft(0L) {
        case (sum, IndexedSeq((x1, y1), (x2, y2))) =>
          sum + x1 * y2 - x2 * y1
      })
      .sum
    val perimeter: Long = lines.map { case ((x1, y1), (x2, y2)) =>
      math.abs(x2 - x1 + y2 - y1)
    }.sum

    private val interiorArea = math.abs(shoelaceSum / 2) - perimeter / 2 + 1
    val area: Long = interiorArea + perimeter
  }

  case class Instruction(dir: Char, dist: Int)

  case class ColoredInstruction(instruction: Instruction, color: String)

  def getPolygonAreaFromInstructions(instructions: Array[Instruction]): Long = {
    Polygon(getPaintedSquareLines(instructions)).area
  }

  def getPointsFromLine(line: Line2D): IndexedSeq[Coord2D] = line match { case ((x1, y1), (x2, y2)) =>
    val points = for {
      x <- math.min(x1, x2) to math.max(x1, x2)
      y <- math.min(y1, y2) to math.max(y1, y2)
    } yield (x, y)

    if (x2 > x1 || y2 > y1) points
    else points.reverse
  }

  def parseColoredInstructions(rawInstructions: Array[String]): Array[ColoredInstruction] = rawInstructions.map {
    case s"$dir $dist (#$color)" => ColoredInstruction(Instruction(dir.head, dist.toInt), color)
  }

  def parseInstructionsFromColors(colors: Array[String]): Array[Instruction] = colors.map(color => {
    val dist = Integer.parseInt(color.dropRight(1), 16)
    val dir = color.last match {
      case '0' => 'R'
      case '1' => 'D'
      case '2' => 'L'
      case '3' => 'U'
      case _ => throw new Error(s"Unexpected direction in color '$color': '${color.last}'")
    }
    Instruction(dir, dist)
  })

  def getPaintedSquareLines(instructions: Array[Instruction]): Array[Line2D] = {
    val lines = mutable.ArrayBuffer[Line2D]()
    var curr = (0L, 0L)

    instructions.foreach { case Instruction(dir, dist) =>
      val line = (curr, dir) match {
        case ((i, j), 'R') => ((i, j), (i, j + dist))
        case ((i, j), 'L') => ((i, j), (i, j - dist))
        case ((i, j), 'U') => ((i, j), (i - dist, j))
        case ((i, j), 'D') => ((i, j), (i + dist, j))
      }
      curr = line._2
      lines.addOne(line)
    }

    lines.toArray
  }
}
