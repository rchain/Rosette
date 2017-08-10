package coop.rchain.rosette

import shapeless._
import shapeless.OpticDefns.RootLens

case class VMState(bytecodes: List[Int],
                   code: Option[Code],
                   ctxt: Option[Ctxt],
                   loc: Location,
                   pc: PC,
                   strandPool: Seq[Ctxt],
                   sigvec: Int = 0,
                   debugLevel: Int = 0,
                   nextOpFlag: Boolean = true,
                   doXmitFlag: Boolean = false,
                   xmitData: (Boolean, Boolean) = (false, false),
                   doRtnFlag: Boolean = false,
                   doNextThreadFlag: Boolean = false,
                   vmErrorFlag: Boolean = false,
                   exitFlag: Boolean = false)
    extends {
  def set[T](f: RootLens[VMState] ⇒ Lens[VMState, T])(value: T): VMState =
    f(lens[VMState]).set(this)(value)

  def update[T](f: RootLens[VMState] ⇒ Lens[VMState, T])(
      value: T => T): VMState =
    f(lens[VMState]).modify(this)(value)
}
