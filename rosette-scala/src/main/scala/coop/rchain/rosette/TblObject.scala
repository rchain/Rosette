package coop.rchain.rosette

case class TblObject(override val entry: Seq[Ob] = null,
                     override val meta: Ob = null,
                     override val slot: Seq[Ob] = null)
    extends Ob(entry, meta, slot) {}

object TblObject {
  object PLACEHOLDER extends TblObject()
}
