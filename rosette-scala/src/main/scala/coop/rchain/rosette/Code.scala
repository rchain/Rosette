package coop.rchain.rosette

class Code extends Ob {
  def lit(l: Int): Ob = Ob.PLACEHOLDER

  def relativize(i: Int) = PC.PLACEHOLDER
}
