package coop.rchain.rosette

case class Ctxt(argvec: Tuple,
                ctxt: Ctxt,
                nargs: Int,
                outstanding: Int,
                tag: Location,
                rslt: Ob,
                env: Env,
                selfEnv: Env,
                reg: Seq[Ob],
                pc: PC,
                code: Code,
                monitor: Monitor)
    extends Ob {
  def parent(): Env = Env.PLACEHOLDER

  def scheduleStrand(state: VMState): VMState =
    state.update(_ >> 'strandPool)(_ :+ this)

  def ret(rslt: Ob): Boolean = true
}

object Ctxt extends Ob {
  object NIV
      extends Ctxt(null,
                   null,
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   new Array[Ob](0),
                   null,
                   null,
                   null)
  object PLACEHOLDER
      extends Ctxt(null,
                   null,
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   new Array[Ob](0),
                   null,
                   null,
                   null)
  def create(tuple: Tuple, ctxt: Ctxt): Ctxt = PLACEHOLDER
}
