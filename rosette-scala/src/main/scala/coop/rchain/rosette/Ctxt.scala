package coop.rchain.rosette

class Ctxt extends Ob {
  var argvec : Tuple
  var ctxt : Option[Ctxt] = None
  var nargs : Int
  var outstanding : Int
  var tag : Location
  var rslt

  def parent() : Option[Env] = {}
  def reg(regno : Int) = {}
  def ret(rslt) : Boolean = {}
  def scheduleStrand() : Unit = {}
}

object Ctxt extends Ob {
  def create(a, ctxt : Ctxt) : Option[Ctxt] = {}
}

