package coop.rchain.rosette.parser.fuzzer

import scala.util.Random

object Fuzzer {
  def fuzzyTermsValid(maxBreadth: Int = 5, maxDepth: Int = 5): Stream[String] =
    Stream.continually(
      FuzzyTerm.randomTerm(Bnf.valid, maxBreadth, maxDepth)(Random.nextLong))

  def fuzzyTermsInvalid(maxBreadth: Int = 5,
                        maxDepth: Int = 5): Stream[String] =
    Stream.continually(
      FuzzyTerm.randomTerm(Bnf.invalid, maxBreadth, maxDepth)(Random.nextLong))
}
