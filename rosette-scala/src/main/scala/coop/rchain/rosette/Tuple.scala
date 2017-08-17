package coop.rchain.rosette

case class Tuple(elem: Seq[Ob]) extends Ob {}

object Tuple {
  object NIL extends Tuple(new Array[Ob](0))
  object PLACEHOLDER extends Tuple(new Array[Ob](0))
  def create(a: Int, b: Ob): Tuple = PLACEHOLDER
}
