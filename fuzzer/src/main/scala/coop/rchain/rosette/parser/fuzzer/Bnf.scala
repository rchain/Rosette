package coop.rchain.rosette.parser.fuzzer

import cats.data.NonEmptyList
import coop.rchain.rosette.parser.fuzzer.Symbols._

object Bnf {
  val grammar = Grammar(
    Seq(
      ProductionRule(Nonterminal(Program),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(Seq(Sym(Nonterminal(Expr), Star))), 1),
                         List.empty
                       ))),
      ProductionRule(
        Nonterminal(Expr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(Seq(Sym(Nonterminal(Free), Once))), 0),
            List((Rhs(Seq(Sym(Nonterminal(Quote), Once))), 8),
                 (Rhs(Seq(Sym(Terminal(String), Star))), 1))
          ))
      ),
      ProductionRule(
        Nonterminal(Free),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(Seq(
               Sym(Terminal(Fix("(free [ ")), Once),
               Sym(Nonterminal(Expr), Star),
               Sym(Terminal(Fix(" ]")), Once),
               Sym(Nonterminal(Expr), Plus),
               Sym(Terminal(Fix(" )")), Once)
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
                              Sym(Terminal(Fix("'")), Once),
                              Sym(Nonterminal(Expr), Once)
                            )),
                          1),
                         List.empty
                       )))
    )
  )
}
