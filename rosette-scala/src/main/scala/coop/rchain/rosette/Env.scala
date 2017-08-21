package coop.rchain.rosette

case class Env(entry: Seq[Ob], meta: Ob, slot: Seq[Ob]) extends Ob {
  def parent(): Env = Env.PLACEHOLDER
  def extendWith(keymeta: Ob): Env = Env.PLACEHOLDER
  def extendWith(keymeta: Ob, argvec: Tuple): Env = Env.PLACEHOLDER
}

object Env {
  object PLACEHOLDER
      extends Env(new Array[Ob](0), Ob.PLACEHOLDER, new Array[Ob](0))
}
