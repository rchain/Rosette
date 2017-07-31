package coop.rchain.rosette.parser.fuzzer

sealed trait ProductionError
case class MissingRule(nt: Nonterminal) extends ProductionError
case object NoTerminalFound extends ProductionError
case object UnexpectedNonterminal extends ProductionError
case object UnexpectedWeight extends ProductionError
