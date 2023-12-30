//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

private[this] final val TargetCutSize = 3

object Day25 {
  type Mat2D = Array[Array[Int]]

  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day25.txt")
    val graph = parseGraph(input)
    val result = findThreeWireCut(graph)
    println(s"Result: $result")
  }

  def parseGraph(input: Array[String]): Mat2D = {
    val nodes = mutable.Set[String]()
    input.foreach { case s"$node: $neighbors" =>
      nodes.addOne(node)
      neighbors.split(" ").foreach(nodes.addOne)
    }

    val nodeIndexes = nodes.toArray.zipWithIndex.map {
      case (nodeName, id) => nodeName -> id
    }.toMap

    val graph = Array.ofDim[Int](nodes.size, nodes.size)
    def addToGraph(node1: Int, node2: Int): Unit = {
      graph(node1)(node2) = 1
      graph(node2)(node1) = 1
    }

    input.foreach { case s"$nodeName: $neighbors" =>
      val node1 = nodeIndexes(nodeName)
      neighbors.split(" ").map(nodeIndexes.get).map(_.get).foreach(node2 => addToGraph(node1, node2))
    }
    graph
  }

  case class Cut(size: Int, nodes: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty[Int])

  case class CandidateCutResult(candidateCut: Cut, s: Int, t: Int)

  def findThreeWireCut(graph: Mat2D): Int = {
    val contractions = graph.indices.map(i => mutable.ArrayBuffer[Int](i))

    @tailrec
    def getCandidateCut(numRemainingIters: Int, weights: Array[Int], s: Int = 0, t: Int = 0): CandidateCutResult = {
      if (numRemainingIters == 0) CandidateCutResult(Cut(weights(t) - graph(t)(t), contractions(t)), s, t)
      else {
        weights(t) = Int.MinValue
        val maxIndex = weights.indexOf(weights.max)

        for (i <- graph.indices) {
          weights(i) += graph(maxIndex)(i)
        }

        getCandidateCut(numRemainingIters - 1, weights, t, maxIndex)
      }
    }

    @tailrec
    def getGroupSize(phase: Int = 1, bestCut: Cut = Cut(Int.MaxValue)): Int = {
      if (phase == graph.length || bestCut.size == TargetCutSize) bestCut.nodes.length
      else {
        val CandidateCutResult(candidateCut, s, t) = getCandidateCut(graph.length - phase, graph.head.clone())

        for (i <- graph.indices) graph(s)(i) += graph(t)(i)
        for (i <- graph.indices) graph(i)(s) = graph(s)(i)
        contractions(s).addAll(contractions(t))
        graph.head(t) = Int.MinValue

        getGroupSize(phase + 1,
          if (candidateCut.size < bestCut.size) candidateCut
          else bestCut
        )
      }
    }

    val groupSize = getGroupSize()
    val otherGroupSize = graph.length - groupSize
    groupSize * otherGroupSize
  }
}
