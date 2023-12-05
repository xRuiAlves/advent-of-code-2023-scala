//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day5 {
  final case class Mapping(destinationRange: Long, sourceRange: Long, rangeLength: Long) {
    def getMappedValueReverse(destination: Long): Option[Long] =
      if (destination >= destinationRange && destination < (destinationRange + rangeLength))
        Some(sourceRange + (destination - destinationRange))
      else None
  }

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day5.txt")

    val inputGroups = input.mkString("\n").split("\n\n")
    val seeds = parseSeeds(inputGroups.head)
    val reverseMappingsGroups = inputGroups.drop(1).map(parseMappings).reverse

    val part1 = findSeed(reverseMappingsGroups, seeds.map(seed => (seed, seed)))
    val part2 = findSeed(
      reverseMappingsGroups,
      seeds
        .grouped(2)
        .map { case Array(seedSource, seedLength) =>
          (seedSource, seedSource + seedLength - 1)
        }
        .toArray
    )

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  def parseSeeds(seedStr: String): Array[Long] = seedStr.stripPrefix("seeds: ").split(" ").map(_.toLong)

  def parseMappingLine(mapLineStr: String): Array[Long] = mapLineStr.split(" ").map(_.toLong)

  def parseMappings(mappingStr: String): Array[Mapping] = mappingStr
    .split("\n")
    .drop(1)
    .map(parseMappingLine)
    .map { case Array(destinationRange, sourceRange, rangeLength) =>
      Mapping(destinationRange, sourceRange, rangeLength)
    }

  def findSeedFromLocation(location: Long, reverseMappingsGroups: Array[Array[Mapping]]): Long =
    reverseMappingsGroups.foldLeft(location)((value, reverseMappingsGroup) =>
      reverseMappingsGroup.map(mapping => mapping.getMappedValueReverse(value)).find(_.isDefined).flatten.getOrElse(value)
    )

  @tailrec
  def findSeed(reverseMappingsGroups: Array[Array[Mapping]], seedRanges: Array[(Long, Long)], location: Long = 0L): Long = {
    val maybeSeed = findSeedFromLocation(location, reverseMappingsGroups)
    if (seedRanges.exists((from, to) => maybeSeed >= from && maybeSeed <= to)) location
    else findSeed(reverseMappingsGroups, seedRanges, location + 1)
  }
}
