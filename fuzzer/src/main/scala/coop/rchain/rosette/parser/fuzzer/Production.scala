package coop.rchain.rosette.parser.fuzzer

import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.either._
import cats.syntax.traverse._
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

case class Rhs(symbols: List[Sym])

case class Sym(symbol: Symbol, occurrence: Occurrence)

case class Grammar(rules: Seq[ProductionRule])

object Production {
  type Weight = Int
  type Seed = Long

  def produce(nt: Nonterminal, maxBreadth: Int, maxDepth: Int)(
      implicit seed: Seed,
      grammar: Grammar): Either[ProductionError, List[Terminal]] =
    try {
      for {
        prodRule <- findProductionRule(nt)
        terminals <- derive(prodRule.alternatives, maxBreadth, maxDepth)
      } yield {
        terminals.map(_.asInstanceOf[Terminal])
      }
    } catch {
      case e: ClassCastException => Left(UnexpectedNonterminal)
    }

  private def derive(alternativeRhs: AlternativeRhs,
                     maxBreadth: Int,
                     maxDepth: Int)(
      implicit seed: Seed,
      grammar: Grammar): Either[ProductionError, List[Symbol]] = {

    // Randomly choose RHS according to weights
    val rhs = randomRhsWeighted(alternativeRhs.value)

    // Expand
    val expanded: List[Symbol] = expandBreadth(rhs, maxBreadth)

    // Derive symbols recursively
    expanded.flatTraverse(
      symbol =>
        deriveRec(symbol, maxBreadth, maxDepth): Either[ProductionError,
                                                        List[Symbol]])
  }

  private def deriveRec(symbol: Symbol, maxBreadth: Int, maxDepth: Int)(
      implicit seed: Seed,
      grammar: Grammar): Either[ProductionError, List[Symbol]] = {
    val rnd = Random
    rnd.setSeed(seed)

    symbol match {
      case t: Terminal => Right(List(t))

      case nt: Nonterminal =>
        val depth = rnd.nextInt(maxDepth + 1)

        if (depth > 0) {
          val rhs = randomRhsWeighted(nt)

          val expanded = rhs.map(r => expandBreadth(r, maxBreadth))

          expanded match {
            case Right(symList) =>
              symList.flatTraverse(
                s =>
                  deriveRec(s, maxBreadth, depth - 1)(rnd.nextLong, grammar): Either[
                    ProductionError,
                    List[Symbol]]): Either[ProductionError, List[Symbol]]

            case Left(error) => Left(error)
          }
        } else {
          findTerminals(nt)
        }
    }
  }

  private def expandBreadth(rhs: Rhs, maxBreadth: Int)(
      implicit seed: Seed): List[Symbol] = {
    val rnd = Random
    rnd.setSeed(seed)

    rhs.symbols.flatMap { sym =>
      sym.occurrence match {
        case Once => List(sym.symbol)
        case Plus => List.fill(rnd.nextInt(maxBreadth) + 1)(sym.symbol)
        case Star => List.fill(rnd.nextInt(maxBreadth))(sym.symbol)
      }
    }
  }

  private def findTerminals(nt: Nonterminal)(
      implicit seed: Seed,
      grammar: Grammar): Either[ProductionError, List[Terminal]] = {
    val rnd = Random
    rnd.setSeed(seed)

    val prodRule: Either[ProductionError, ProductionRule] = findProductionRule(
      nt)

    prodRule match {
      case Right(rule) =>
        val rhs = randomRhsWeighted(rule.alternatives.value)

        rhs.symbols.flatTraverse(sym =>
          sym.symbol match {
            case t: Terminal =>
              Right(List(t)): Either[ProductionError, List[Terminal]]
            case nt: Nonterminal =>
              fixedFindTerminals(nt)(rnd.nextLong, grammar): Either[
                ProductionError,
                List[Terminal]]
        })

      case Left(error) => Left(error)
    }
  }

  private def fixedFindTerminals(nt: Nonterminal)(
      implicit seed: Seed,
      grammar: Grammar): Either[ProductionError, List[Terminal]] = {
    val rnd = Random
    rnd.setSeed(seed)

    val rhs = randomRhsWeighted(nt)

    val symbols = rhs.map(_.symbols)

    symbols.map(_.flatMap { sym: Sym =>
      sym.symbol match {
        case nt: Nonterminal =>
          nt.symbol match {
            case Expr => List(randomConstant()(rnd.nextLong))
            case Pattern =>
              List(Terminal(Fix("[ ")),
                   randomConstant()(rnd.nextLong),
                   Terminal(Fix(" ]")))
            case _ => List(randomConstant()(rnd.nextLong))
          }
        case t: Terminal => List(t)
      }
    })
  }

  private def randomConstant()(implicit seed: Seed): Terminal = {
    val rnd = Random
    rnd.setSeed(seed)

    rnd.nextInt(6) match {
      case 0 => Terminal(RFixnum)
      case 1 => Terminal(RString)
      case 2 => Terminal(RBoolean)
      case 3 => Terminal(RFloat)
      case 4 => Terminal(RChar)
      case 5 => Terminal(REscape)
    }
  }

  private def findProductionRule(nt: Nonterminal)(
      implicit grammar: Grammar): Either[ProductionError, ProductionRule] = {
    val rule = grammar.rules.find(rule => rule.lhs.symbol == nt.symbol)

    rule match {
      case Some(r) => Right(r)
      case None => Left(MissingRule(nt))
    }
  }

  private def randomRhsWeighted(nt: Nonterminal)(
      implicit seed: Seed,
      grammar: Grammar): Either[ProductionError, Rhs] = {
    val rule = findProductionRule(nt)(grammar)
    rule.map(r => randomRhsWeighted(r.alternatives.value))
  }

  private def randomRhsWeighted(
      weightedRhs: NonEmptyList[(Rhs, Production.Weight)])(
      implicit seed: Seed): Rhs = {
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
