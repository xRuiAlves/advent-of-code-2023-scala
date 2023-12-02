//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day2 {
  val MaxRedCubes = 12
  val MaxGreenCubes = 13
  val MaxBlueCubes = 14

  final case class Game(id: Int, info: Array[Array[CubeInfo]]) {
    private final def countCubes(color: String): Int = info.map(_.filter(_.color == color).map(_.count).sum).max
    private lazy final val redCubes = countCubes("red")
    private lazy final val greenCubes = countCubes("green")
    private lazy final val blueCubes = countCubes("blue")

    final val isValid: Boolean = (
      redCubes <= MaxRedCubes &&
      greenCubes <= MaxGreenCubes &&
      blueCubes <= MaxBlueCubes
    )

    final val power: Int = redCubes * greenCubes * blueCubes
  }
  
  final case class CubeInfo(color: String, count: Int)

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day2.txt")

    val part1 = input
      .map(parseGame)
      .filter(_.isValid)
      .map(_.id)
      .sum

    val part2 = input
      .map(parseGame)
      .map(_.power)
      .sum

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def parseGame(game: String): Game = Game(
    game.split(": ").head.split(" ").last.toInt,
    game.split(": ").last.split("; ").map(_.split(", ").map {
      case s"$count $color" => CubeInfo(color, count.toInt)
    })
  )
}
