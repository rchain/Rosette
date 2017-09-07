package coop.rchain.rosette

case class TblObject(override val entry: Seq[Ob],
                     override val meta: Ob,
                     override val slot: Seq[Ob])
    extends Ob

object TblObject {
  object PLACEHOLDER extends TblObject(null, null, null)
}
