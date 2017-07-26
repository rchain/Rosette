package coop.rchain.rosette.parser.fuzzer

import coop.rchain.rosette.parser.fuzzer.Symbols._

object Bnf {
  val grammar = Grammar(
    Seq(
      ProductionRule(Nonterminal(Program),
                     AlternativeRhs(
                       Seq(
                         (Rhs(Seq((Nonterminal(Expr), Star))), 1)
                       ))),
      ProductionRule(
        Nonterminal(Expr),
        AlternativeRhs(
          Seq(
            (Rhs(Seq((Nonterminal(Free), Once))), 1),
            (Rhs(Seq((Nonterminal(Quote), Once))), 1),
            (Rhs(Seq((Terminal(String), Once))), 1)
          ))
      ),
      ProductionRule(
        Nonterminal(Free),
        AlternativeRhs(
          Seq(
            (Rhs(
               Seq(
                 (Terminal(Fix("(free [ ")), Once),
                 (Nonterminal(Expr), Star),
                 (Terminal(Fix(" ]")), Once),
                 (Nonterminal(Expr), Plus),
                 (Terminal(Fix(" )")), Once)
               )),
             1)
          ))
      ),
      ProductionRule(Nonterminal(Quote),
                     AlternativeRhs(
                       Seq(
                         (Rhs(
                            Seq(
                              (Terminal(Fix("'")), Once),
                              (Nonterminal(Expr), Once)
                            )),
                          1)
                       )))
    )
  )
}
