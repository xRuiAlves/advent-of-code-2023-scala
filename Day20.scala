//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.collection.mutable

object Day20 {
  private[this] final val SequenceInitialModule = "broadcaster"
  private[this] final val SequenceTargetModule = "rx"

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day20.txt")
    val modules = parseModules(input)

    val part2 = applySequence(modules)
    println(s"Part 2: $part2")
  }

  enum Pulse {
    case LOW, HIGH, NONE
  }

  abstract class Module(val outputs: Array[String]) {
    def applyPulse(from: String, input: Pulse): Pulse
  }

  case class StatelessModule(override val outputs: Array[String]) extends Module(outputs) {
    override def applyPulse(from: String, input: Pulse): Pulse = Pulse.LOW
  }

  case class FlipFlopModule(override val outputs: Array[String]) extends Module(outputs) {
    private var isOn: Boolean = false

    override def applyPulse(from: String, input: Pulse): Pulse = input match {
      case Pulse.HIGH => Pulse.NONE
      case Pulse.LOW => if (isOn) {
        isOn = false
        Pulse.LOW
      } else {
        isOn = true
        Pulse.HIGH
      }
      case other => other
    }
  }

  case class ConjunctionModule(override val outputs: Array[String], inputs: Array[String]) extends Module(outputs) {
    private val memory = inputs
      .map(input => input -> Pulse.LOW)
      .to(mutable.Map)

    override def applyPulse(from: String, input: Pulse): Pulse = {
      memory(from) = input
      if (memory.values.forall(_ == Pulse.HIGH)) Pulse.LOW
      else Pulse.HIGH
    }
  }

  def parseModules(input: Array[String]): Map[String, Module] = {
    val modules = mutable.Set[(String, String)]()
    val moduleOutputs = mutable.Map[String, Array[String]]()
    val moduleInputs = mutable.Map[String, mutable.ArrayBuffer[String]]()

    def addInputsAndOutputs(inputStr: String, outputsStr: String): Unit = {
      val outputs = outputsStr.split(", ")
      moduleOutputs(inputStr) = outputs
      outputs.foreach(output => {
        val inputs = moduleInputs.getOrElse(output, mutable.ArrayBuffer[String]())
        inputs.addOne(inputStr)
        moduleInputs(output) = inputs
      })
    }

    input.foreach {
      case s"%$input -> $output" =>
        modules.addOne(("FlipFlop", input))
        addInputsAndOutputs(input, output)
      case s"&$input -> $output" =>
        modules.addOne(("Conjunction", input))
        addInputsAndOutputs(input, output)
      case s"$input -> $output" =>
        modules.addOne(("Stateless", input))
        addInputsAndOutputs(input, output)
    }

    modules.map {
      case ("Stateless", moduleName) => moduleName -> StatelessModule(moduleOutputs(moduleName))
      case ("FlipFlop", moduleName) => moduleName -> FlipFlopModule(moduleOutputs(moduleName))
      case ("Conjunction", moduleName) => moduleName -> ConjunctionModule(
        moduleOutputs(moduleName),
        moduleInputs(moduleName).toArray
      )
    }.toMap
  }

  def applySequence(modules: Map[String, Module]): (Int, Int) = {
    var lowPulses = 0
    var highPulses = 0
    val toVisit = mutable.Queue[(String, String, Pulse)]()

    for (numButtonPresses <- 1 to Int.MaxValue) {
      if (numButtonPresses == 1000) {
        println(s"Part 1: ${lowPulses * highPulses}")
      }

      toVisit.enqueue((SequenceInitialModule, SequenceInitialModule, Pulse.LOW))

      while (toVisit.nonEmpty) {
        val (fromModuleId, toModuleId, pulse) = toVisit.dequeue()

        if (toModuleId == SequenceTargetModule && pulse == Pulse.LOW) {
          return numButtonPresses
        }

        if (pulse == Pulse.LOW) {
          lowPulses += 1
        }
        if (pulse == Pulse.HIGH) {
          highPulses += 1
        }

        if (modules.contains(toModuleId) && pulse != Pulse.NONE) {
          val module = modules(toModuleId)
          val output = module.applyPulse(fromModuleId, pulse)

          module.outputs.foreach(targetModule => {
            toVisit.enqueue((toModuleId, targetModule, output))
          })
        }
      }
    }

    throw new Error("Failed to find a solution! :(")
  }
}
