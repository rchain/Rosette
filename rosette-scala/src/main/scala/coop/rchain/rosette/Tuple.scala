package coop.rchain.rosette

case class Tuple(elem: Seq[Ob],
                 override val entry: Seq[Ob],
                 override val meta: Ob,
                 override val slot: Seq[Ob])
    extends Ob

object Tuple {
  def apply(a: Int, b: Option[Ob]): Tuple =
    new Tuple(null, null, null, null)

  val PLACEHOLDER = apply(0, None)
}
