package coop.rchain.rosette.parser.fuzzer

import java.util.UUID
import coop.rchain.rosette.parser.fuzzer.Symbols._
import scala.util.Random

object FuzzyTerm {
  def randomTerm(grammar: Grammar, maxBreadth: Int, maxDepth: Int)(
      seed: Long): String = {
    val rnd = Random
    rnd.setSeed(seed)

    val terminals =
      Production.produce(Nonterminal(Program), maxBreadth, maxDepth)(seed,
                                                                     grammar)

    terminals match {
      case Right(ts) =>
        ts.map(terminal => randomGroundTerm(terminal.symbol)(rnd.nextLong))
          .mkString("")
      case Left(error) => s"Error: $error"
    }
  }

  private def randomGroundTerm(terminal: TerminalSym, truncate: Int = 5)(
      seed: Long): String = {
    val rnd = Random
    rnd.setSeed(seed)

    terminal match {
      case Fix(value) => value
      case Id =>
        "\"" + UUID
          .randomUUID()
          .toString
          .replace("-", "")
          .substring(0, truncate) + "\""
      case RString =>
        "\"" + UUID
          .randomUUID()
          .toString
          .replace("-", "")
          .substring(0, truncate) + "\""
      case RFixnum => rnd.nextInt(Int.MaxValue).toString
      case RBoolean => if (rnd.nextBoolean()) "#t" else "#f"
      case RFloat => rnd.nextFloat().toString
      case RChar => "#\\a"
      case REscape => "#\\\\xff"
    }
  }
}
