//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day19 {
  type PartRange = (Char, Int, Int)

  private[this] final val StartWorkflowId = "in"
  private[this] final val Accepted = "A"
  private[this] final val Rejected = "R"
  private[this] final val LessThan = '<'
  private[this] final val GreaterThan = '>'
  private[this] final val MinCategoryValue = 1
  private[this] final val MaxCategoryValue = 4000

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day19.txt")
    val inputSections = input.mkString("\n").split("\n\n").map(_.split("\n"))
    val workflows = parseWorkflows(inputSections.head)
    val parts = parseParts(inputSections.last)

    val part1 = evalParts(parts, workflows)
      .filter(_._2 == true)
      .map(_._1.value)
      .sum
    val part2 = evalWorkflows(workflows)

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  case class Part(x: Int, m: Int, a: Int, s: Int) {
    final lazy val value: Int = x + m + a + s
  }

  case class Workflow(id: String, rules: Array[Rule])

  abstract class Rule(val targetWorkflow: String) {
    def eval(part: Part): Boolean
  }

  case class ComparisonRule(
      partSection: Char,
      comparisonType: Char,
      comparisonValue: Int,
      override val targetWorkflow: String
  ) extends Rule(targetWorkflow) {
    override def eval(part: Part): Boolean = {
      val partValue = partSection match {
        case 'x' => part.x
        case 'm' => part.m
        case 'a' => part.a
        case 's' => part.s
      }

      comparisonType match {
        case LessThan    => partValue < comparisonValue
        case GreaterThan => partValue > comparisonValue
      }
    }
  }

  case class RedirectRule(override val targetWorkflow: String) extends Rule(targetWorkflow) {
    override def eval(part: Part): Boolean = true
  }

  def parseWorkflows(rawWorkflows: Array[String]): Map[String, Workflow] = rawWorkflows.map { case s"$id{$rulesStr}" =>
    val rules = rulesStr.split(",").map {
      case s"$partSection<$comparisonValue:$targetWorkflow" =>
        ComparisonRule(
          partSection.head,
          LessThan,
          comparisonValue.toInt,
          targetWorkflow
        )
      case s"$partSection>$comparisonValue:$targetWorkflow" =>
        ComparisonRule(
          partSection.head,
          GreaterThan,
          comparisonValue.toInt,
          targetWorkflow
        )
      case targetWorkflow => RedirectRule(targetWorkflow)
    }

    val workflow = Workflow(id, rules)
    id -> workflow
  }.toMap

  def parseParts(rawParts: Array[String]): Array[Part] = rawParts.map { case s"{x=$x,m=$m,a=$a,s=$s}" =>
    Part(x.toInt, m.toInt, a.toInt, s.toInt)
  }

  @tailrec
  def evalPart(part: Part, workflows: Map[String, Workflow], currWorkflowId: String): Boolean = {
    val targetWorkflow = workflows(currWorkflowId).rules
      .find(rule => rule.eval(part))
      .map(_.targetWorkflow)
      .get

    if (targetWorkflow == Accepted) true
    else if (targetWorkflow == Rejected) false
    else evalPart(part, workflows, targetWorkflow)
  }

  def evalParts(parts: Array[Part], workflows: Map[String, Workflow]): Map[Part, Boolean] = parts
    .map(part => part -> evalPart(part, workflows, StartWorkflowId))
    .toMap

  def updatedRanges(
      ranges: Array[PartRange],
      partSection: Char,
      comparisonType: Char,
      comparisonValue: Int
  ): Array[PartRange] = ranges
    .map {
      case (part, pi, pf) if part != partSection => (part, pi, pf)
      case (part, pi, pf) if part == partSection =>
        if (comparisonType == LessThan) {
          if (pi >= comparisonValue) (part, -1, -1)
          else (part, pi, math.min(pf, comparisonValue - 1))
        } else {
          if (pf <= comparisonValue) (part, -1, -1)
          else (part, math.max(pi, comparisonValue + 1), pf)
        }
    }
    .filter(_._2 != -1)

  def evalWorkflows(workflows: Map[String, Workflow]): Long = {
    var validValues = 0L

    def evalWorkflows(currWorkflowId: String, candidateRanges: Array[PartRange]): Unit = {
      if (currWorkflowId == Accepted) {
        validValues += candidateRanges.map { case (_, pi, pf) =>
          (pf - pi + 1).toLong
        }.product
      } else if (currWorkflowId == Rejected) {} else {
        var filteredRanges = candidateRanges

        workflows(currWorkflowId).rules.foreach {
          case RedirectRule(targetWorkflowId) => evalWorkflows(targetWorkflowId, filteredRanges)
          case ComparisonRule(partSection, comparisonType, comparisonValue, targetWorkflow) =>
            evalWorkflows(targetWorkflow, updatedRanges(filteredRanges, partSection, comparisonType, comparisonValue))
            filteredRanges = comparisonType match {
              case GreaterThan => updatedRanges(filteredRanges, partSection, LessThan, comparisonValue + 1)
              case LessThan    => updatedRanges(filteredRanges, partSection, GreaterThan, comparisonValue - 1)
            }
        }
      }
    }

    evalWorkflows(
      StartWorkflowId,
      Array(
        ('x', MinCategoryValue, MaxCategoryValue),
        ('m', MinCategoryValue, MaxCategoryValue),
        ('a', MinCategoryValue, MaxCategoryValue),
        ('s', MinCategoryValue, MaxCategoryValue)
      )
    )

    validValues
  }
}
