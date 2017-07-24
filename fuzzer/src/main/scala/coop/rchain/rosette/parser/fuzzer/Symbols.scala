package coop.rchain.rosette.parser.fuzzer

object Symbols {
  sealed trait TerminalSym
  case object Id extends TerminalSym
  case object Fixnum extends TerminalSym
  case object String extends TerminalSym
  case object Boolean extends TerminalSym
  case object Float extends TerminalSym
  case object Char extends TerminalSym
  case object Escape extends TerminalSym

  sealed trait NonterminalSym
  case object Start extends NonterminalSym
  case object Quote extends NonterminalSym
  case object Free extends NonterminalSym
}
