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
    extends Ob {
  def scheduleStrand(): Unit = {}
  def ret(rslt: Ob): Boolean = true
}

object Ctxt {
  def apply(tuple: Option[Tuple], ctxt: Ctxt): Ctxt = PLACEHOLDER

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
}
