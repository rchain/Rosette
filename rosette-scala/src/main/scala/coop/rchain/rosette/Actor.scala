package coop.rchain.rosette

case class Actor(extension: Ob,
                 override val entry: Seq[Ob],
                 override val meta: Ob,
                 override val slot: Seq[Ob])
    extends Ob

object Actor {
  def apply(extension: Ob) =
    new Actor(extension, extension.entry, extension.meta, extension.slot)
}
