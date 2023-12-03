//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

object Day3 {
  final val EMPTY_SPACE = '.'

  def main(args: Array[String]): Unit = {
    val matrix = readResourceLines("day3.txt").map(_.toCharArray)

    def isInBounds(i: Int, j: Int): Boolean = {
      i >= 0 && j >= 0 && i < matrix.length && j < matrix(i).length
    }

    def getValue(i: Int, j: Int): Char = if (isInBounds(i, j)) matrix(i)(j) else EMPTY_SPACE

    def floodFillDigitsToEmpty(i: Int, j: Int): Unit = if (isInBounds(i, j) && getValue(i, j).isDigit) {
      matrix(i)(j) = EMPTY_SPACE
      getNeighbours(i, j).foreach {
        case (neighborI, neighborJ) => floodFillDigitsToEmpty(neighborI, neighborJ)
      }
    }

    def parseNumber(i: Int, j: Int): Int = matrix(i).drop(j).takeWhile(_.isDigit).mkString.toInt

    def getNeighbours(i: Int, j: Int): Array[(Int, Int)] = Array(
      (i - 1, j - 1),
      (i - 1, j),
      (i - 1, j + 1),
      (i, j - 1),
      (i, j + 1),
      (i + 1, j - 1),
      (i + 1, j),
      (i + 1, j + 1),
    )

    def hasSymbolNeighbor(i: Int, j: Int): Boolean = getNeighbours(i, j).exists {
      case (neighborI, neighborJ) => {
        val neighbor = getValue(neighborI, neighborJ)
        !neighbor.isDigit && neighbor != EMPTY_SPACE
      }
    }

    def getPartNumber(i: Int, j: Int): Int = if (!getValue(i, j).isDigit) 0 else {
      val isPartNumber = matrix(i).zipWithIndex.drop(j).takeWhile(_._1.isDigit).map(_._2).exists {
        case digitJ => hasSymbolNeighbor(i, digitJ)
      }

      val partNumber = if (isPartNumber) parseNumber(i, j) else 0
      floodFillDigitsToEmpty(i, j)
      partNumber
    }

    var part1 = 0

    for (
      i <- matrix.indices;
      j <- matrix(i).indices
    ) {
      part1 += getPartNumber(i, j)
    }

    println(s"Part 1: $part1")
  }

}
