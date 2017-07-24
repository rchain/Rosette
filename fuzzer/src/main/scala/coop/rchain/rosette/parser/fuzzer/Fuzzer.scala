package coop.rchain.rosette.parser.fuzzer

import java.util.UUID

import coop.rchain.rosette.parser.fuzzer.Symbols._

import scala.util.Random

object Fuzzer extends App {
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

  def getPositiveTestFile: String = "pos.txt"

  def getNegativeTestFile: String = "neg.txt"

  val terminals = Production.produce(Nonterminal(Start))(
    posGrammar,
    scala.util.Random.nextLong())

  println("Terminals for posGrammar: " + terminals)

}

trait FuzzyTerm {

  def randomGroundTerm(terminal: TerminalSym,
                       rnd: Random,
                       truncate: Int = 5): String =
    terminal match {
      case Id =>
        "\"" + UUID
          .randomUUID()
          .toString
          .replace("-", "")
          .substring(0, truncate) + "\""
      case String =>
        "\"" + UUID
          .randomUUID()
          .toString
          .replace("-", "")
          .substring(0, truncate) + "\""
      case Fixnum => rnd.nextInt(Int.MaxValue).toString
      case Boolean => if (rnd.nextBoolean()) "#t" else "#f"
      case Float => rnd.nextFloat().toString
      case Char => "#\\a"
      case Escape => "#\\\\xff"
    }
}
