package coop.rchain.rosette

class Prim extends Ob {
  def dispatchHelper(ctxt: Ctxt): Ob = Ob.PLACEHOLDER
}

object Prim {
  val nthPrim: Seq[Prim] = new Array[Prim](0)
}
