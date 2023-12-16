//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLine

import scala.collection.mutable

object Day15 {
  private[this] final val ASCII_MOD_SPACE = 256
  private[this] final val ASCII_MULTIPLIER = 17

  case class Instruction(private val instructionStr: String) {
    val label: String = instructionStr.takeWhile(_.isLetter)
    val focalLength: Int = instructionStr.filter(_.isDigit).toIntOption.getOrElse(-1)
    val hash: Int = getHash(label)
  }

  case class Box(label: String, focalLength: Int)

  def main(args: Array[String]): Unit = {
    val input = readResourceLine("day15.txt")
    val words = input.split(",")
    val instructions = words.map(word => Instruction(word))

    val part1 = words.map(getHash).sum
    val part2 = getBoxes(instructions).zipWithIndex.map { case (box, boxIdx) => getBoxValue(box, boxIdx) }.sum

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def getBoxes(instructions: Array[Instruction]): Array[Array[Box]] = {
    val boxes = (0 until ASCII_MOD_SPACE).map(_ => mutable.ArrayBuffer[Box]()).toArray

    instructions.foreach(instruction => {
      val idx = boxes(instruction.hash).indexWhere(_.label == instruction.label)
      if (instruction.focalLength == -1) {
        if (idx != -1) boxes(instruction.hash).remove(idx)
      } else {
        val node = Box(instruction.label, instruction.focalLength)
        if (idx != -1) boxes(instruction.hash)(idx) = node
        else boxes(instruction.hash).addOne(node)
      }
    })

    boxes.map(_.toArray)
  }

  def getHash(word: String): Int = word.toCharArray
    .foldLeft(0)((acc, curr) => (acc + curr) * ASCII_MULTIPLIER % ASCII_MOD_SPACE)

  def getBoxValue(box: Array[Box], boxIdx: Int): Int = box.zipWithIndex.map { case (boxNode, boxNodeIdx) =>
    (boxIdx + 1) * (boxNodeIdx + 1) * boxNode.focalLength
  }.sum
}
