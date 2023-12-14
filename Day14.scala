//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec

object Day14 {
  type Mat2D = Array[Array[Char]]

  private[this] final val RoundRock = 'O';
  private[this] final val SquareRock = '#';
  private[this] final val EmptySpace = '.';

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day14.txt")
    val map = input.map(_.toCharArray)

    map.foreach(a => {
      a.foreach(b => {
        print(s"$b ")
      })
      println()
    })

    tiltMap(map)
    println()
    println()
    println()

    map.transpose.map(_.reverse).foreach(a => {
      a.foreach(b => {
        print(s"$b ")
      })
      println()
    })

    val part1 = map
      .transpose
      .map(_
        .reverse
        .zipWithIndex
        .map((cell, i) =>
          if (cell == RoundRock) i + 1
          else 0
        ).
        sum
      )
      .sum
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def tiltMap(map: Mat2D): Unit = map.head.indices.foreach(i => tiltMapCol(map, i))

  @tailrec
  def tiltMapCol(map: Mat2D, col: Int): Unit = {
    var swapped = false

    map.indices.sliding(2).foreach { case IndexedSeq(i1, i2) => {
      if (map(i1)(col) == EmptySpace && map(i2)(col) == RoundRock) {
        swapped = true
        map(i1)(col) = RoundRock
        map(i2)(col) = EmptySpace
      }
    }}

    if (swapped) {
      tiltMapCol(map, col)
    }
  }
}
