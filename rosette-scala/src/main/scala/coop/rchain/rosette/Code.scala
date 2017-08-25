package coop.rchain.rosette

case class Code(litvec: Tuple) extends Ob {
  def lit(l: Int): Ob = litvec.elem(l)
}
