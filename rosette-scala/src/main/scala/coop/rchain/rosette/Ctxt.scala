package coop.rchain.rosette

case class Ctxt(argvec: Tuple,
                ctxt: Ctxt,
                nargs: Int,
                outstanding: Int,
                tag: Location,
                rslt: Ob,
                env: Env,
                reg: Seq[Ob],
                pc: PC)
    extends Ob {
  def parent(): Env = Env.PLACEHOLDER;
  def scheduleStrand(): Unit = {}
  def ret(rslt: Ob): Boolean = true
}

object Ctxt extends Ob {
  object NIV
      extends Ctxt(null, null, 0, 0, null, null, null, new Array[Ob](0), null)
  object PLACEHOLDER
      extends Ctxt(null, null, 0, 0, null, null, null, new Array[Ob](0), null)
  def create(tuple: Tuple, ctxt: Ctxt): Ctxt = PLACEHOLDER
}
