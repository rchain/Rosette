package coop.rchain.rosette

trait RblAtom extends Ob

case class Fixnum(value: Int,
                  override val meta: Ob = null,
                  override val slot: Seq[Ob] = null)
    extends RblAtom
