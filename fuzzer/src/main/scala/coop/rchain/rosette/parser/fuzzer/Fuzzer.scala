package coop.rchain.rosette.parser.fuzzer

import scala.util.Random

object Fuzzer {
  val fuzzyTerms: Stream[String] =
    Stream.continually(
      FuzzyTerm.randomTerm(Bnf.grammar, 1, 1)(Random.nextLong))

  def main(args: Array[String]): Unit =
    fuzzyTerms.take(1).foreach(println(_))

  def getPositiveTestFile: String = "pos.txt"

  def getNegativeTestFile: String = "neg.txt"
}
