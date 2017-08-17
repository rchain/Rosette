package coop.rchain.rosette

case class Location(atom: Ob) extends Ob {
  // def store(ctxt: Ctxt, rslt: Ob): Boolean = true;
}

object Location {
  object PLACEHOLDER extends Location(Ob.PLACEHOLDER)
  object LIMBO extends Location(Ob.PLACEHOLDER)

  def ArgReg(a: Int): Location = PLACEHOLDER
  def CtxtReg(r: Int): Location = PLACEHOLDER
  def fetch(loc: Location, k: Ctxt): Ob = Ob.PLACEHOLDER
  def store(loc: Location, k: Ctxt, value: Ob): Boolean = true
}
