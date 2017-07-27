package coop.rchain.rosette.parser.fuzzer

import cats.data.NonEmptyList
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

case class AlternativeRhs(value: NonEmptyList[(Rhs, Production.Weight)])

case class Rhs(symbols: Seq[Sym])

case class Sym(symbol: Symbol, occurrence: Occurrence)

case class Grammar(rules: Seq[ProductionRule])

object Production {
  type Weight = Int

  /*
   * Return random production for a nonterminal
   */
  def produce(nt: Nonterminal, maxBreadth: Int, depth: Int)(
      implicit grammar: Grammar,
      seed: Long): Either[ProductionError, Seq[Terminal]] =
    try {
      for {
        prodRule <- findProductionRule(nt)
        rhsTerminals <- derive(prodRule.alternatives, maxBreadth, depth)
      } yield {
        rhsTerminals.symbols.map(_.symbol.asInstanceOf[Terminal])
      }
    } catch {
      case e: ClassCastException => Left(UnexpectedNonterminal)
    }

  /*
   * Derive random production from RHS by following production rules from grammar
   * Will return MissingRule error if there is no production rule for a nonterminal
   */
  private def derive(alternativeRhs: AlternativeRhs,
                     maxBreadth: Int,
                     depth: Int)(implicit grammar: Grammar,
                                 seed: Long): Either[ProductionError, Rhs] = {
    /* Randomly choose RHS
     * If depth = 0, choose path to terminal
     */
    val randomRhs = chooseRhs(alternativeRhs, depth)(seed)

    val derivedSymbols = randomRhs.map(rhs =>
      rhs.symbols.map(sym =>
        sym.symbol match {
          case nt: Nonterminal =>
            val prodRule = findProductionRule(nt)

            prodRule match {
              case Right(rule) =>
                val rhsEither =
                  derive(rule.alternatives, maxBreadth, depth - 1)

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

          case Terminal(_) => Seq(sym)
      }))

    derivedSymbols match {
      case Right(derivedSyms) =>
        if (derivedSyms.exists(_.isEmpty)) {
          Left(MissingRule)
        } else {
          Right(Rhs(derivedSyms.flatten))
        }
      case Left(error) => Left(error)
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

  private def expandBreadth(syms: Seq[Sym], maxBreadth: Int)(
      seed: Long): Seq[Sym] = {
    val rnd = Random

    syms.flatMap { sym =>
      sym.occurrence match {
        case Once => Seq(sym)
        case Plus => Seq.fill(rnd.nextInt(maxBreadth) + 1)(sym)
        case Star => Seq.fill(rnd.nextInt(maxBreadth))(sym)
      }
    }
  }

  private def chooseRhs(alternatives: AlternativeRhs, depth: Int)(
      seed: Long): Either[ProductionError, Rhs] =
    if (depth > 0) {
      Right(chooseRhsWeighted(alternatives.value)(seed))
    } else {
      // Choose RHS which leads to a terminal
      chooseRhsTerminals(alternatives)(seed)
    }

  private def chooseRhsTerminals(alternativeRhs: AlternativeRhs)(
      seed: Long): Either[ProductionError, Rhs] = {
    val possibleRhs = alternativeRhs.value.filter {
      case (rhs, _) => isTerminalRhs(rhs)
    }

    if (possibleRhs.nonEmpty) {
      val nonEmptyPossibleRhs =
        NonEmptyList(possibleRhs.head, possibleRhs.tail)
      Right(chooseRhsWeighted(nonEmptyPossibleRhs)(seed))
    } else {
      Left(NoTerminalFound)
    }
  }

  private def isTerminalRhs(rhs: Rhs): Boolean =
    rhs.symbols.forall(sym => sym.symbol.isInstanceOf[Terminal])

  private def chooseRhsWeighted(
      weightedRhs: NonEmptyList[(Rhs, Production.Weight)])(seed: Long): Rhs = {
    val rnd = Random
    rnd.setSeed(seed)
    val p = rnd.nextFloat

    val weightSum = weightedRhs.foldLeft(0) {
      case (s, (_, weight)) => s + weight
    }

    // Inverse CDF
    val rhs = weightedRhs.zipWithIndex
      .find {
        case (elem, i) =>
          val cdf = weightedRhs.toList.take(i + 1).foldLeft(0.0) {
            case (s, (_, weight)) => s + weight / weightSum.toFloat
          }

          if (p <= cdf) true else false
      }
      // This is ugly but fine since weightedRhs can't be empty
      .get
      ._1
      ._1

    rhs
  }
}
