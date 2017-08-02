package coop.rchain.rosette.parser.fuzzer

import cats.data.NonEmptyList
import coop.rchain.rosette.parser.fuzzer.Symbols._

object Bnf {
  val valid = Grammar(
    Seq(
      // Program ::= Expr
      ProductionRule(Nonterminal(Program),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(List(Sym(Nonterminal(Expr), Once))), 1),
                         List.empty
                       ))),

      /* Expr ::= Method | Rmethod | Quote | Label | Block | Seq | Let | Letrec | If | Proc | Free | Goto
       *          | Set | Constant | Request | Send
       */
      ProductionRule(
        Nonterminal(Expr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(List(Sym(Nonterminal(Method), Once))), 1),
            List(
              (Rhs(List(Sym(Nonterminal(Rmethod), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Quote), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Label), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Block), Once))), 1),
              (Rhs(List(Sym(Nonterminal(SeqExpr), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Let), Once))), 1),
              (Rhs(List(Sym(Nonterminal(LetRec), Once))), 1),
              (Rhs(List(Sym(Nonterminal(If), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Proc), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Free), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Goto), Once))), 1),
              (Rhs(List(Sym(Nonterminal(SetExpr), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Constant), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Request), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Send), Star))), 1),
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
                 Sym(Terminal(Fix("] ")), Once),
               )),
             1),
            List(
              (Rhs(
                 List(
                   Sym(Terminal(Fix(" [")), Once),
                   Sym(Nonterminal(Expr), Star),
                   Sym(Terminal(Fix(" & ")), Once),
                   Sym(Nonterminal(Expr), Once),
                   Sym(Terminal(Fix("] ")), Once),
                 )),
               1)
            )
          ))
      ),

      // Rmethod ::= '(rmethod' Pattern Expr+ ')'
      ProductionRule(
        Nonterminal(Rmethod),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(rmethod ")), Once),
                 Sym(Nonterminal(Pattern), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
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
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Quote ::= '\'' Expr
      ProductionRule(Nonterminal(Quote),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(
                            List(
                              Sym(Terminal(Fix("'")), Once),
                              Sym(Nonterminal(Expr), Once),
                            )),
                          1),
                         List.empty
                       ))),

      // Label ::= '(label' Id Expr+ ')'
      ProductionRule(
        Nonterminal(Label),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(label ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Block ::= '(block' Expr+ ')'
      ProductionRule(
        Nonterminal(Block),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(block ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Seq ::= '(seq' Expr+ ')'
      ProductionRule(
        Nonterminal(SeqExpr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(seq ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // LetHead ::= ('[' Id Expr ']' | '[' pattern expr ']')
      ProductionRule(
        Nonterminal(LetHead),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("[ ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ]")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("[ ")), Once),
                 Sym(Nonterminal(Pattern), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ]")), Once),
               )),
             1)
            )
          ))
      ),

      // Let ::= '(let [' LetHead* ']' Expr+ ')'
      ProductionRule(
        Nonterminal(Let),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(let [ ")), Once),
                 Sym(Nonterminal(LetHead), Star),
                 Sym(Terminal(Fix(" ] ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // LetRecHead ::= '[' Id Expr ']'
      ProductionRule(
        Nonterminal(LetRecHead),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("[ ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ]")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // LetRec ::= '(letrec [' LetRecHead* ']' Expr+ ')'
      ProductionRule(
        Nonterminal(LetRec),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(letrec [ ")), Once),
                 Sym(Nonterminal(LetRecHead), Star),
                 Sym(Terminal(Fix(" ] ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // If ::= '(if' Expr Expr ')' | '(if' Expr Expr Expr ')'
      ProductionRule(
        Nonterminal(If),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(if ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" )")), Once),
               )),
             1),
            List(
             (Rhs(
               List(
                 Sym(Terminal(Fix("(if ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" )")), Once),
               )),
             1)
            )
          ))
      ),

      // Proc ::= '(proc' Pattern Expr ')'
      ProductionRule(
        Nonterminal(Proc),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(proc ")), Once),
                 Sym(Nonterminal(Pattern), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Free ::= '(free [' Id* ']' Expr+ ')'
      ProductionRule(
        Nonterminal(Free),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(free [ ")), Once),
                 Sym(Terminal(Id), Star),
                 Sym(Terminal(Fix(" ] ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Goto ::= '(goto' Id ')'
      ProductionRule(
        Nonterminal(Goto),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(goto ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Set ::= '(set!' Id Expr ')'
      ProductionRule(
        Nonterminal(SetExpr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(set! ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Constant ::= RString | RBoolean | RFixnum | RFloat | RChar | REscape | RAbsent | REof | RNiv | ReadError | IncompleteIo
      ProductionRule(
        Nonterminal(Constant),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(RString), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(RBoolean), Once),
               )),
             1),
             (Rhs(
               List(
                 Sym(Terminal(RFixnum), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(RFloat), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(RChar), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(REscape), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(RAbsent), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(REof), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(RNiv), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(ReadError), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(IncompleteIo), Once),
               )),
             1)
            )
          ))
      ),


      // Clause ::= Expr* | Expr* '&' Expr
      ProductionRule(
        Nonterminal(Clause),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Nonterminal(Expr), Star),
               )),
             1),
            List(
             (Rhs(
               List(
                 Sym(Nonterminal(Expr), Star),
                 Sym(Terminal(Fix(" & ")), Once),
                 Sym(Nonterminal(Expr), Once),
               )),
             1)
            )
          ))
      ),

      // Request ::= '(' Expr Clause ')'
      ProductionRule(
        Nonterminal(Request),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Clause), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Send ::= '(send' Expr Clause ')'
      ProductionRule(
        Nonterminal(Send),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(send ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Clause), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),
    )
  )

  val invalid = Grammar(
    Seq(
      // Program ::= Expr
      ProductionRule(Nonterminal(Program),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(List(Sym(Nonterminal(Expr), Once))), 1),
                         List.empty
                       ))),

      /** Expr ::= Method | Rmethod | Quote | Label | Block | Seq | Let | Letrec | If | Proc | Free | Goto
        *          | Set | Constant | Request | Send
        */
      ProductionRule(
        Nonterminal(Expr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(List(Sym(Nonterminal(Method), Once))), 1),
            List(
              (Rhs(List(Sym(Nonterminal(Rmethod), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Quote), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Label), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Block), Once))), 1),
              (Rhs(List(Sym(Nonterminal(SeqExpr), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Let), Once))), 1),
              (Rhs(List(Sym(Nonterminal(LetRec), Once))), 1),
              (Rhs(List(Sym(Nonterminal(If), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Proc), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Free), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Goto), Once))), 1),
              (Rhs(List(Sym(Nonterminal(SetExpr), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Constant), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Request), Once))), 1),
              (Rhs(List(Sym(Nonterminal(Send), Star))), 1),
            )
          ))
      ),

      // Pattern ::= '[' Expr* ']' | '[' Expr* '&' Expr ']'
      ProductionRule(
        Nonterminal(Pattern),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix(" [")), Once),
                 Sym(Nonterminal(Expr), Star),
                 Sym(Terminal(Fix("] ")), Once),
               )),
             1),
            List(
              (Rhs(
                 List(
                   Sym(Terminal(Fix(" [")), Once),
                   Sym(Nonterminal(Expr), Star),
                   Sym(Terminal(Fix(" & ")), Once),
                   Sym(Nonterminal(Expr), Once),
                   Sym(Terminal(Fix("] ")), Once),
                 )),
               1)
            )
          ))
      ),

      /** Rmethod
        *
        * Valid: Rmethod ::= '(rmethod' Pattern Expr+ ')'
        * Invalid: Rmethod ::= '(rmethod' Expr Expr* ')'
        */
      ProductionRule(
        Nonterminal(Rmethod),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(rmethod ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Star),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      /** Method
        *
        * Valid: Method ::= '(method' Pattern Expr+ ')'
        * Invalid: Method ::= '(method' Expr Expr* ')'
        */
      ProductionRule(
        Nonterminal(Method),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(method ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Star),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      /** Quote
        *
        * Valid: Quote ::= '\'' Expr
        * Invalid: -
        */
      ProductionRule(Nonterminal(Quote),
                     AlternativeRhs(
                       NonEmptyList(
                         (Rhs(
                            List(
                              Sym(Terminal(Fix("'")), Once),
                              Sym(Nonterminal(Expr), Once),
                            )),
                          1),
                         List.empty
                       ))),

      /** Label
        *
        * Valid: Label ::= '(label' Id Expr+ ')'
        * Invalid: Label ::= '(label' Id ')' | '(label' Constant Expr* ')'
        */
      ProductionRule(
        Nonterminal(Label),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(label ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("(label ")), Once),
                 Sym(Nonterminal(Constant), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Star),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            )
          ))
      ),

      /** Block
        *
        * Valid: Block ::= '(block' Expr+ ')'
        * Invalid: Block ::= '(block)'
        */
      ProductionRule(
        Nonterminal(Block),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(block)")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("(block ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            )
          ))
      ),

      /** Seq
        *
        * Valid: Seq ::= '(seq' Expr+ ')'
        * Invalid: Seq ::= '(seq)'
        */
      ProductionRule(
        Nonterminal(SeqExpr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(seq)")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("(seq ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            )
          ))
      ),

      /** LetHead
        *
        * Valid: LetHead ::= ('[' Id Expr ']' | '[' Pattern Expr ']')
        * Invalid: LetHead ::= ('[' Expr ']' | '[' Expr Expr Expr+ ']')
        */
      ProductionRule(
        Nonterminal(LetHead),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("[ ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ]")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("[ ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(" ]")), Once),
               )),
             1)
            )
          ))
      ),

      /** Let
        *
        * Valid: Let ::= '(let [' LetHead* ']' Expr+ ')'
        * Invalid: Let ::= '(let' LetHead* Expr+ ')' | '(let [' LetHead* '])'
        */
      ProductionRule(
        Nonterminal(Let),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(let ")), Once),
                 Sym(Nonterminal(LetHead), Star),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("(let [ ")), Once),
                 Sym(Nonterminal(LetHead), Star),
                 Sym(Terminal(Fix(" ]")), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            )
          ))
      ),

      /** LetRecHead
        *
        * Valid: LetRecHead ::= '[' Id Expr ']'
        * Invalid: LetRecHead ::= '[' Expr ']'
        */
      ProductionRule(
        Nonterminal(LetRecHead),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("[ ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ]")), Once),
               )),
             1),
            List.empty
          ))
      ),

      /** LetRec
        *
        * Valid: LetRec ::= '(letrec [' LetRecHead* ']' Expr+ ')'
        * Invalid: LetRec ::= '(letrec)' | '(letrec' LetRecHead* Expr+ ')' | '(letrec [' LetRecHead* '])'
        */
      ProductionRule(
        Nonterminal(LetRec),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(letrec)")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("(letrec ")), Once),
                 Sym(Nonterminal(LetRecHead), Star),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(Fix("(letrec [ ")), Once),
                 Sym(Nonterminal(LetRecHead), Star),
                 Sym(Terminal(Fix(" ]")), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            )
          ))
      ),

      /** If
        *
        * Valid: If ::= '(if' Expr Expr ')' | '(if' Expr Expr Expr ')'
        * Invalid: If ::= '(if' Expr Expr Expr Expr+ ')'
        */
      ProductionRule(
        Nonterminal(If),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(if ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(" )")), Once),
               )),
             1),
            List.empty
          ))
      ),

      /** Proc
        *
        * Valid: Proc ::= '(proc' Pattern Expr ')'
        * Invalid: Proc ::= '(proc)' | '(proc' Id Expr ')' | '(proc' Pattern ')'
        */
      ProductionRule(
        Nonterminal(Proc),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(proc)")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("(proc ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(Fix("(proc ")), Once),
                 Sym(Nonterminal(Pattern), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            )
          ))
      ),

      /** Free
        *
        * Valid: Free ::= '(free [' Id* ']' Expr+ ')'
        * Invalid: Free ::= '(free [' Id* '])' | '(free [' Constant* ']' Expr+ ')' | '(free' Expr* ')'
        */
      ProductionRule(
        Nonterminal(Free),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(free [ ")), Once),
                 Sym(Terminal(Id), Star),
                 Sym(Terminal(Fix(" ]")), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(Fix("(free [ ")), Once),
                 Sym(Nonterminal(Constant), Star),
                 Sym(Terminal(Fix(" ] ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(Fix("(free ")), Once),
                 Sym(Nonterminal(Expr), Star),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            )
          ))
      ),

      /** Goto
        *
        * Valid: Goto ::= '(goto' Id ')'
        * Invalid: Goto ::= '(goto' Constant ')' | '(goto' Id Expr+ ')' | '(goto)'
        */
      ProductionRule(
        Nonterminal(Goto),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(goto ")), Once),
                 Sym(Nonterminal(Constant), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List(
             (Rhs(
               List(
                 Sym(Terminal(Fix("(goto ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(Fix("(goto)")), Once),
               )),
             1),
          )))
      ),

      /** Set
        *
        * Valid: Set ::= '(set!' Id Expr ')'
        * Invalid: Set ::= '(set!)' | '(set!' Id ')' | '(set!' Constant Expr ')' | '(set!' Id Expr Expr+ ')'
        */
      ProductionRule(
        Nonterminal(SetExpr),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(set!)")), Once),
               )),
             1),
            List(
             (Rhs(
               List(
                 Sym(Terminal(Fix("(set! ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(Fix("(set! ")), Once),
                 Sym(Nonterminal(Constant), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(Fix("(set! ")), Once),
                 Sym(Terminal(Id), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Expr), Plus),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            )
          ))
      ),

      /** Constant ::= RString | RBoolean | RFixnum | RFloat | RChar | REscape | RAbsent | REof | RNiv | ReadError
        *              | IncompleteIo
        */
      ProductionRule(
        Nonterminal(Constant),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(RString), Once),
               )),
             1),
            List(
              (Rhs(
               List(
                 Sym(Terminal(RBoolean), Once),
               )),
             1),
             (Rhs(
               List(
                 Sym(Terminal(RFixnum), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(RFloat), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(RChar), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(REscape), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(RAbsent), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(REof), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(RNiv), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(ReadError), Once),
               )),
             1),
              (Rhs(
               List(
                 Sym(Terminal(IncompleteIo), Once),
               )),
             1)
            )
          ))
      ),


      // Clause ::= Expr* | Expr* '&' Expr
      ProductionRule(
        Nonterminal(Clause),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Nonterminal(Expr), Star),
               )),
             1),
            List(
             (Rhs(
               List(
                 Sym(Nonterminal(Expr), Star),
                 Sym(Terminal(Fix(" & ")), Once),
                 Sym(Nonterminal(Expr), Once),
               )),
             1)
            )
          ))
      ),

      // Request ::= '(' Expr Clause ')'
      ProductionRule(
        Nonterminal(Request),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Clause), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      ),

      // Send ::= '(send' Expr Clause ')'
      ProductionRule(
        Nonterminal(Send),
        AlternativeRhs(
          NonEmptyList(
            (Rhs(
               List(
                 Sym(Terminal(Fix("(send ")), Once),
                 Sym(Nonterminal(Expr), Once),
                 Sym(Terminal(Fix(" ")), Once),
                 Sym(Nonterminal(Clause), Once),
                 Sym(Terminal(Fix(")")), Once),
               )),
             1),
            List.empty
          ))
      )
    )
  )
}
