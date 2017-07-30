package coop.rchain.rosette.parser.fuzzer

import cats.data.NonEmptyList
import coop.rchain.rosette.parser.fuzzer.Symbols._

object Bnf {
  val grammar = Grammar(
    Seq(
      // Program ::= Expr+
      ProductionRule(Nonterminal(Program),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(List(Sym(Nonterminal(Expr), Once))), 1),
                         List.empty
                       ))),
      /* Expr ::= Method | Rmethod | Quote | Label | String | Block | Seq | Let | Letrec | If | Proc | Free | Goto
       *          | Set | Constant | Id | Request | Send
       */
      ProductionRule(
        Nonterminal(Expr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(List(Sym(Nonterminal(Method), Once))), 1),
            List(
              //(Rhs(List(Sym(Nonterminal(Rmethod), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Quote), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Label), Once))), 1),
              //(Rhs(List(Sym(Terminal(RString), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Block), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(SeqExpr), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Let), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Letrec), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(If), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Proc), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Free), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Goto), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(SetExpr), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Constant), Once))), 1),
              //(Rhs(List(Sym(Terminal(Id), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Request), Once))), 1),
              //(Rhs(List(Sym(Nonterminal(Send), Star))), 1)
            )
          ))
      ),
      // Pattern ::= '[' Expr* ']' | '[' Expr* '&' Expr ']' ;
      ProductionRule(
        Nonterminal(Pattern),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix(" [")), Once),
                 Sym(Nonterminal(Expr), Star),
                 Sym(Terminal(Fix("] ")), Once)
               )),
             1),
            List(
              (Rhs(
                 List(
                   Sym(Terminal(Fix(" [")), Once),
                   Sym(Nonterminal(Expr), Star),
                   Sym(Terminal(Fix(" & ")), Once),
                   Sym(Nonterminal(Expr), Once),
                   Sym(Terminal(Fix("] ")), Once)
                 )),
               1)
            )
          ))
      ),
      // Method ::= '(method' Pattern Expr+ ')'
      ProductionRule(
        Nonterminal(Method),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(method ")), Once),
                 Sym(Nonterminal(Pattern), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once)
               )),
             1),
            List.empty
          ))
      ),
      ProductionRule(Nonterminal(Quote),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(
                            List(
                              Sym(Terminal(Fix("'")), Once),
                              Sym(Nonterminal(Expr), Once)
                            )),
                          1),
                         List.empty
                       )))
    )
  )
}
