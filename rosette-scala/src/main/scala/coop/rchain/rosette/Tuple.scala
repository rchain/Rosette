package coop.rchain.rosette

case class Tuple() extends Ob {}

object Tuple {
  //def create(a, b : Option[?]) : Tuple = {}

  // Dummy
  def create(a: Int, b: Option[Int]): Tuple = Tuple()
}
