package coop.rchain.rosette

case class PC(relative: Int, absolute: Int) {
  //def fetch(): Instr = {}
}

object PC {
  object PLACEHOLDER extends PC(0, 0)
  def fromInt(i: Int): PC = PLACEHOLDER
}
