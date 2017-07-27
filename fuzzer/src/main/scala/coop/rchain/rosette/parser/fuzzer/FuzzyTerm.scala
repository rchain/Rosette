package coop.rchain.rosette.parser.fuzzer

import java.util.UUID
import coop.rchain.rosette.parser.fuzzer.Symbols._
import scala.util.Random

object FuzzyTerm {
  def randomTerm(grammar: Grammar, maxBreadth: Int, depth: Int)(
      seed: Long): String = {
    val terminals =
      Production.produce(Nonterminal(Program), maxBreadth, depth)(grammar,
                                                                  seed)

    terminals match {
      case Right(ts) =>
        ts.map(terminal => randomGroundTerm(terminal.symbol)(seed))
          .mkString(" ")
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
}
