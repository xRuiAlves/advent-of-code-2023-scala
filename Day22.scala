//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day22 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day22.txt")
    val bricks = input.map(rawBrickLine => Brick.fromStr(rawBrickLine))

    val bricksAfterFall = simulateBricksFall(bricks).bricksAfterFall
    val fallenBricksCounts = bricksAfterFall.map(brickToRemove =>
      simulateBricksFall(bricksAfterFall.filterNot(_.equals(brickToRemove))).numFallenBricks
    )

    val part1 = fallenBricksCounts.count(_ == 0)
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  case class Coord3D(x: Int, y: Int, z: Int) {
    def fall(): Coord3D = Coord3D(x, y, z - 1)
  }

  case class Brick(cubes: Set[Coord3D]) {
    def fall(): Brick = Brick(cubes.map(_.fall()))
  }

  object Brick {
    def fromStr(rawBrickLine: String): Brick = Brick((rawBrickLine match
      case s"$x0,$y0,$z0~$x1,$y1,$z1" => for {
        x <- x0.toInt to x1.toInt
        y <- y0.toInt to y1.toInt
        z <- z0.toInt to z1.toInt
      } yield Coord3D(x, y, z)
    ).toSet)
  }

  case class SimulationResult(bricksAfterFall: Array[Brick], numFallenBricks: Int)

  def simulateBricksFall(bricks: Array[Brick]): SimulationResult = {
    val bricksOrderedFromFloor = bricks.sortBy(_.cubes.map(_.z).min)
    val fallenCubes = mutable.Set[Coord3D]()
    val bricksAfterSimulation = mutable.ArrayBuffer[Brick]()

    @tailrec
    def simulateBrickFall(brick: Brick): Brick = {
      val brickAfterFall = brick.fall()
      if (brickAfterFall.cubes.exists(cube => fallenCubes.contains(cube) || cube.z <= 0)) brick
      else simulateBrickFall(brickAfterFall)
    }

    val numFallenBricks = bricksOrderedFromFloor.count(brick => {
      val brickAfterFall = simulateBrickFall(brick)
      bricksAfterSimulation.addOne(brickAfterFall)
      fallenCubes.addAll(brickAfterFall.cubes)
      !brickAfterFall.equals(brick)
    })

    SimulationResult(bricksAfterSimulation.toArray, numFallenBricks)
  }
}
