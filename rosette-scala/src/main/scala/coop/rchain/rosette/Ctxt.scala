package coop.rchain.rosette

/*
class Ctxt extends Ob {
  var argvec : Tuple
  var ctxt : Option[Ctxt] = None
  var nargs : Int
  var outstanding : Int
  var tag : Location
  var rslt
  var reg: List[Int]

  def parent() : Option[Env] = {}
  def ret(rslt) : Boolean = {}
  def scheduleStrand() : Unit = {}
}
 */

// Add rslt
case class Ctxt(argvec: Tuple,
                ctxt: Option[Ctxt],
                nargs: Int,
                outstanding: Int,
                tag: Location)
    extends Ob {
  //def parent(): Option[Env] = {}
  //def reg(regno : Int) = {}
  //def ret(rslt) : Boolean = {}
  //def scheduleStrand() : Unit = {}
}

object Ctxt extends Ob {
  //def create(a, ctxt : Ctxt) : Option[Ctxt] = {}

  // Dummy
  def create(tuple: Option[Tuple], ctxt: Ctxt): Option[Ctxt] = None
}
