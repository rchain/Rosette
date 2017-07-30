package coop.rchain.rosette.parser.fuzzer

object Symbols {
  sealed trait TerminalSym
  case class Fix(value: String) extends TerminalSym
  case object Id extends TerminalSym
  case object RFixnum extends TerminalSym
  case object RString extends TerminalSym
  case object RBoolean extends TerminalSym
  case object RFloat extends TerminalSym
  case object RChar extends TerminalSym
  case object REscape extends TerminalSym

  sealed trait NonterminalSym
  case object Program extends NonterminalSym
  case object Expr extends NonterminalSym
  case object Method extends NonterminalSym
  case object Rmethod extends NonterminalSym
  case object Quote extends NonterminalSym
  case object Label extends NonterminalSym
  case object Block extends NonterminalSym
  case object SeqExpr extends NonterminalSym
  case object Let extends NonterminalSym
  case object Letrec extends NonterminalSym
  case object If extends NonterminalSym
  case object Proc extends NonterminalSym
  case object Free extends NonterminalSym
  case object Goto extends NonterminalSym
  case object SetExpr extends NonterminalSym
  case object Constant extends NonterminalSym
  case object Request extends NonterminalSym
  case object Send extends NonterminalSym
  case object Pattern extends NonterminalSym
}
