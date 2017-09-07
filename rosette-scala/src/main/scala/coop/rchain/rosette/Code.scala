package coop.rchain.rosette

case class Code(override val entry: Seq[Ob],
                override val meta: Ob,
                override val slot: Seq[Ob])
    extends Ob {
  def lit(l: Int): Ob = null
}
