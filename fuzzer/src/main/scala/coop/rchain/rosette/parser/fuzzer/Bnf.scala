package coop.rchain.rosette.parser.fuzzer

import cats.data.NonEmptyList
import coop.rchain.rosette.parser.fuzzer.Symbols._

object Bnf {
  val grammar = Grammar(
    Seq(
      ProductionRule(Nonterminal(Program),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(Seq((Nonterminal(Expr), Star))), 1),
                         List.empty
                       ))),
      ProductionRule(
        Nonterminal(Expr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(Seq((Nonterminal(Free), Once))), 1),
            List((Rhs(Seq((Nonterminal(Quote), Once))), 1),
                 (Rhs(Seq((Terminal(String), Once))), 1))
          ))
      ),
      ProductionRule(
        Nonterminal(Free),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               Seq(
                 (Terminal(Fix("(free [ ")), Once),
                 (Nonterminal(Expr), Star),
                 (Terminal(Fix(" ]")), Once),
                 (Nonterminal(Expr), Plus),
                 (Terminal(Fix(" )")), Once)
               )),
             1),
            List.empty
          ))
      ),
      ProductionRule(Nonterminal(Quote),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(
                            Seq(
                              (Terminal(Fix("'")), Once),
                              (Nonterminal(Expr), Once)
                            )),
                          1),
                         List.empty
                       )))
    )
  )
}
