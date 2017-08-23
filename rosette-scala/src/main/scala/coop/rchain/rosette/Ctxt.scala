package coop.rchain.rosette

import shapeless._
import shapeless.OpticDefns.RootLens

case class Ctxt(argvec: Tuple,
                ctxt: Ctxt,
                nargs: Int,
                outstanding: Int,
                tag: Location,
                rslt: Ob,
                env: Ob,
                selfEnv: Ob,
                reg: Seq[Ob],
                pc: PC)
    extends Ob {
  def scheduleStrand(): Unit = {}
  def ret(rslt: Ob): Boolean = true

  def set[T](f: RootLens[Ctxt] ⇒ Lens[Ctxt, T])(value: T): Ctxt =
    f(lens[Ctxt]).set(this)(value)

  def update[T](f: RootLens[Ctxt] ⇒ Lens[Ctxt, T])(value: T => T): Ctxt =
    f(lens[Ctxt]).modify(this)(value)
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
                   null)
  def create(tuple: Tuple, ctxt: Ctxt): Ctxt = PLACEHOLDER
}
