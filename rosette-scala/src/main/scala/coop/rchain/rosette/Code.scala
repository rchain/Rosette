package coop.rchain.rosette

case class Code(override val meta: Ob, override val slot: Seq[Ob]) extends Ob {
  def lit(l: Int): Ob = null
}
