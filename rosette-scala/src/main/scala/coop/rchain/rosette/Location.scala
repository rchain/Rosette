package coop.rchain.rosette

class Location extends Ob {
  var atom : Option[Ob]
}

object Location {
  def ArgReg(a : Int) : Option[Location] = {}
  def CtxtReg(r : Int) : Option[Location] = {}
}

