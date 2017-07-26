package coop.rchain.rosette.parser.fuzzer

import coop.rchain.rosette.parser.fuzzer.Symbols._

import scala.util.Random

object Fuzzer {
  val posGrammar = Grammar(
    Seq(
      ProductionRule(Nonterminal(Start),
                     AlternativeRhs(
                       Seq(
                         (Rhs(Seq((Terminal(Fixnum), Plus))), 1),
                         (Rhs(Seq((Terminal(String), Star))), 1)
                       )))
    )
  )

  val fuzzyTerms: Stream[String] =
    Stream.continually(FuzzyTerm.randomTerm(posGrammar, 2)(Random.nextLong))

  def main(args: Array[String]): Unit =
    fuzzyTerms.take(5).foreach(println(_))

  def getPositiveTestFile: String = "pos.txt"

  def getNegativeTestFile: String = "neg.txt"
}
