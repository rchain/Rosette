package coop.rchain.rosette.parser.fuzzer

import scala.util.Random

object Fuzzer {
  val fuzzyTerms: Stream[String] =
    Stream.continually(
      FuzzyTerm.randomTerm(Bnf.grammar, 2, 8)(Random.nextLong))

  def main(args: Array[String]): Unit =
    fuzzyTerms.take(4).foreach(println(_))

  def getPositiveTestFile: String = "pos.txt"

  def getNegativeTestFile: String = "neg.txt"
}
