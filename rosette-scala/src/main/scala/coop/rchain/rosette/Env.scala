package coop.rchain.rosette

case class Env(entry: Seq[Ob]) extends Ob {
  def parent(): Env = Env.PLACEHOLDER
}
object Env {
  object PLACEHOLDER extends Env(new Array[Ob](0))
}
