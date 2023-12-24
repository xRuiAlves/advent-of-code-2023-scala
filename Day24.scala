//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day24 {
//  private[this] final val CollisionAreaMin = 7L
//  private[this] final val CollisionAreaMax = 27L
  private[this] final val CollisionAreaMin = 200000000000000L
  private[this] final val CollisionAreaMax = 400000000000000L

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day24.txt")
    val lines = input.map(Line.fromStr)
    val linePairs = for {
      i <- lines.indices
      j <- (i + 1) until lines.length
    } yield (lines(i), lines(j))

    val part1 = linePairs.count { case (line1, line2) => line1.intersectsInPlaneInFuture(
      line2, CollisionAreaMin, CollisionAreaMax, CollisionAreaMin, CollisionAreaMax
    )}
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  case class Line(x: Long, y: Long, z: Long, vx: Long, vy: Long, vz: Long) {
    private def findIntersection(other: Line): Option[(Double, Double)] = {
      val d = vx * other.vy - other.vx * vy

      if (d == 0) None
      else {
        val t = ((other.x - x) * other.vy + (y - other.y) * other.vx) / d.toDouble
        val xIntersection = x + vx * t
        val yIntersection = y + vy * t
        Some((xIntersection, yIntersection))
      }
    }

    private def isIntersectionInFuture(intersectionX: Double, intersectionY: Double): Boolean = {
      if (vx > 0 && intersectionX < x) false
      else if (vy > 0 && intersectionY < y) false
      else if (vx < 0 && intersectionX > x) false
      else if (vy < 0 && intersectionY > y) false
      else true
    }


    def intersectsInPlaneInFuture(other: Line, minX: Long, maxX: Long, minY: Long, maxY: Long): Boolean = findIntersection(other) match
      case None => false
      case Some((x, y)) =>
        x >= minX && x <= maxX && y >= minY && y <= maxY && this.isIntersectionInFuture(x, y) && other.isIntersectionInFuture(x, y)
  }

  object Line {
    def fromStr(lineStr: String): Line = lineStr.replaceAll("  ", " ") match
      case s"$x, $y, $z @ $vx, $vy, $vz" => Line(x.toLong, y.toLong, z.toLong, vx.toLong, vy.toLong, vz.toLong)
  }
}
