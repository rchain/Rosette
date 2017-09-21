package coop.rchain.rosette

case class Ctxt(argvec: Tuple,
                code: Code,
                ctxt: Ctxt,
                env: Ob,
                override val meta: Ob,
                monitor: Monitor,
                nargs: Int,
                outstanding: Int,
                pc: PC,
                reg: Seq[Ob],
                rslt: Ob,
                selfEnv: Ob,
                override val slot: Seq[Ob],
                tag: Location)
    extends Ob {
  def ret(rslt: Ob): Boolean = true

  def scheduleStrand(state: VMState): VMState =
    state.update(_ >> 'strandPool)(_ :+ this)

}

object Ctxt {
  def apply(tuple: Option[Tuple], ctxt: Ctxt): Ctxt = PLACEHOLDER

  object NIV
      extends Ctxt(null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null)
  object PLACEHOLDER
      extends Ctxt(null,
                   null,
                   null,
                   null,
                   null,
                   null,
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null)
}
