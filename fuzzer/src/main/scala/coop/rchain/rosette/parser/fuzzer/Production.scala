package coop.rchain.rosette.parser.fuzzer

import coop.rchain.rosette.parser.fuzzer.Symbols._
import scala.util.Random

sealed trait Symbol
case class Nonterminal(symbol: NonterminalSym) extends Symbol
case class Terminal(symbol: TerminalSym) extends Symbol

sealed trait Occurrence
case object Once extends Occurrence
case object Plus extends Occurrence
case object Star extends Occurrence

case class ProductionRule(lhs: Nonterminal, alternatives: AlternativeRhs)

case class AlternativeRhs(value: Seq[(Rhs, Production.Weight)])

case class Rhs(symbols: Seq[(Symbol, Occurrence)])

case class Grammar(rules: Seq[ProductionRule])

object Production {
  type Weight = Int

  /*
   * Return random production for a nonterminal
   */
  def produce(nt: Nonterminal)(
      implicit grammar: Grammar,
      seed: Long): Either[ProductionError, Seq[Terminal]] =
    try {
      for {
        prodRule <- findProductionRule(nt)
        rhsTerminals <- derive(prodRule.alternatives, 5, 5)
      } yield {
        rhsTerminals.symbols.map(_._1.asInstanceOf[Terminal])
      }
    } catch {
      case e: ClassCastException => Left(UnexpectedNonterminal)
    }

  /*
   * Derive random production from rhs by following production rules from grammar
   * Will return MissingRule error if there is no production rule for a nonterminal
   */
  private def derive(alternativeRhs: AlternativeRhs,
                     maxBreadth: Int,
                     maxDepth: Int)(
      implicit grammar: Grammar,
      seed: Long): Either[ProductionError, Rhs] = {
    val randomRhs = chooseRhs(alternativeRhs)(seed)

    val derivedSymbols = randomRhs.symbols.map(elem =>
      elem._1 match {
        case nt: Nonterminal =>
          val prodRule = findProductionRule(nt)

          prodRule match {
            case Right(rule) =>
              // Choose random (respecting weight) Rhs
              val rhsEither =
                derive(rule.alternatives, maxBreadth, maxDepth - 1)

              rhsEither match {
                case Right(recRhs) =>
                  // Randomly expand breadth on symbols
                  expandBreadth(recRhs.symbols, maxBreadth)(seed)

                case Left(error) =>
                  // Empty Seq means missing production rule
                  Seq()
              }

            case Left(error) =>
              // Empty Seq means missing production rule
              Seq()
          }

        case Terminal(_) => Seq(elem)
    })

    if (derivedSymbols.exists(_.isEmpty)) {
      Left(MissingRule)
    } else {
      Right(Rhs(derivedSymbols.flatten))
    }
  }

  private def findProductionRule(nt: Nonterminal)(
      implicit grammar: Grammar): Either[ProductionError, ProductionRule] = {
    val rule = grammar.rules.find(rule => rule.lhs.symbol == nt.symbol)

    rule match {
      case Some(r) => Right(r)
      case None => Left(MissingRule)
    }
  }

  private def expandBreadth(
      symbols: Seq[(Symbol, Occurrence)],
      maxBreadth: Int)(seed: Long): Seq[(Symbol, Occurrence)] = {
    val rnd = Random

    symbols.flatMap {
      case (sym, occurrence) =>
        occurrence match {
          case Once => Seq((sym, occurrence))
          case Plus => Seq.fill(rnd.nextInt(maxBreadth) + 1)((sym, occurrence))
          case Star => Seq.fill(rnd.nextInt(maxBreadth))((sym, occurrence))
        }
    }
  }

  private def chooseRhs(alternatives: AlternativeRhs)(seed: Long): Rhs = {
    val rnd = Random
    rnd.setSeed(seed)
    val p = rnd.nextFloat

    val weightSum = alternatives.value.foldLeft(0) {
      case (s, (_, weight)) => s + weight
    }

    // Inverse CDF
    val rhs = alternatives.value.zipWithIndex.find {
      case (elem, i) =>
        val cdf = alternatives.value.take(i + 1).foldLeft(0.0) {
          case (s, (_, weight)) => s + weight / weightSum.toFloat
        }

        if (p <= cdf) true else false
    }

    // Fix
    rhs.get._1._1
  }
}
