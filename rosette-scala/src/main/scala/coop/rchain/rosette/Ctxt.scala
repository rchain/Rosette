package coop.rchain.rosette

case class Ctxt(argvec: Tuple,
                ctxt: Ctxt,
                override val entry: Seq[Ob],
                env: Ob,
                override val meta: Ob,
                nargs: Int,
                outstanding: Int,
                pc: PC,
                reg: Seq[Ob],
                rslt: Ob,
                selfEnv: Ob,
                override val slot: Seq[Ob],
                tag: Location)
    extends Ob(entry, meta, slot) {
  def scheduleStrand(): Unit = {}
  def ret(rslt: Ob): Boolean = true
}

object Ctxt extends Ob {
  object NIV
      extends Ctxt(null,
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
                   0,
                   0,
                   null,
                   null,
                   null,
                   null,
                   null,
                   null)
  def create(tuple: Tuple, ctxt: Ctxt): Ctxt = PLACEHOLDER
}
