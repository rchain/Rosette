package coop.rchain.rosette.parser.fuzzer

import java.util.UUID

import coop.rchain.rosette.parser.fuzzer.Production.Seed
import coop.rchain.rosette.parser.fuzzer.Symbols._

import scala.util.Random

object FuzzyTerm {
  def randomTerm(grammar: Grammar, maxBreadth: Int, maxDepth: Int)(
      implicit seed: Seed): String = {
    val rnd = Random
    rnd.setSeed(seed)

    val terminals =
      Production.produce(Nonterminal(Program), maxBreadth, maxDepth)(seed,
                                                                     grammar)

    terminals match {
      case Right(ts) =>
        ts.map(terminal => randomGroundTerm(terminal.symbol)(rnd.nextLong))
          .mkString("")
      case Left(MissingRule(nt)) => s"Missing rule for symbol: ${nt.symbol}"
      case Left(error) => s"Error: $error"
    }
  }

  private def randomGroundTerm(terminal: TerminalSym, truncate: Int = 5)(
      implicit seed: Seed): String = {
    val rnd = Random
    rnd.setSeed(seed)

    terminal match {
      case Fix(value) => value
      case Id => randomId().substring(0, truncate)
      case RString =>
        "\"" + UUID
          .randomUUID()
          .toString
          .replace("-", "")
          .substring(0, truncate) + "\""
      case RFixnum => rnd.nextInt(Int.MaxValue).toString
      case RFloat => rnd.nextFloat().toString
      case RBoolean => if (rnd.nextBoolean()) "#t" else "#f"
      case RChar => randomChar()
      case REscape => randomEscape()
      case RAbsent => "#absent"
      case REof => "#eof"
      case RNiv => "#niv"
      case ReadError => "#read-error"
      case IncompleteIo => "incomplete-io"
    }
  }

  /** Generates a random Id according to the Rosette manual */
  private def randomId()(implicit seed: Seed): String = {
    val rnd = Random
    rnd.setSeed(seed)

    val char = (('a' to 'z') ++ ('A' to 'Z')).map(_.toString)
    val digit = (0 to 9).map(_.toString)
    val extendedAlphabeticChars =
      Seq('+', '-', '*', '/', '<', '=', '>', '!', '?', '$', '%', '_', '~', '^')
        .map(_.toString)

    val all = char ++ digit ++ extendedAlphabeticChars

    val first: String = char(rnd.nextInt(char.size))
    val rest: Stream[String] = Stream.continually(all(rnd.nextInt(all.size)))

    first + rest.take(31).foldLeft("")((s, c) => s + c)
  }

  private def randomChar()(implicit seed: Seed): String = {
    val rnd = Random
    rnd.setSeed(seed)

    val chars = '0' to 'z'

    "#\\" + chars(rnd.nextInt(chars.size))
  }

  private def randomEscape()(implicit seed: Seed): String = {
    val rnd = Random
    rnd.setSeed(seed)

    val hexChars = ('a' to 'f').map(_.toString) ++ ('A' to 'F').map(_.toString) ++ (0 to 9)
      .map(_.toString)
    val hex = "#\\\\x" + hexChars(rnd.nextInt(hexChars.size)) + hexChars(
      rnd.nextInt(hexChars.size))

    val escapes = Seq("#\\\\n", "#\\\\r", "#\\\\t", "#\\\\f", "#\\\\\"") :+ hex

    escapes(rnd.nextInt(escapes.size))
  }
}
