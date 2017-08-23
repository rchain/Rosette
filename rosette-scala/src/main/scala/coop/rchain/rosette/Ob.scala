package coop.rchain.rosette

import shapeless._
import shapeless.OpticDefns.RootLens

sealed trait LookupError
case object Absent extends LookupError
case object Upcall extends LookupError

class Ob {
  val entry: Seq[Ob] = null
  val meta: Ob = null
  val slot: Seq[Ob] = null

  def container(): Ob = Ob.PLACEHOLDER
  def extendWith(keymeta: Ob): Ob = Ob.PLACEHOLDER
  def extendWith(keymeta: Ob, argvec: Tuple): Ob = Ob.PLACEHOLDER
  def is(value: Ob.ObTag): Boolean = true
  def lookupOBO(meta: Ob, ob: Ob, key: Ob): Either[LookupError, Ob] =
    Right(Ob.PLACEHOLDER)
  def numberOfSlots(): Int = 0
  def parent(): Ob = Ob.PLACEHOLDER
  def setAddr(ind: Int, level: Int, offset: Int, value: Ob): Option[Ob] = None
  def setLex(ind: Int, level: Int, offset: Int, value: Ob): Option[Ob] = None
}

object Ob {
  object PLACEHOLDER extends Ob
  object NIV extends Ob
  object FALSE extends Ob
  object DEADTHREAD extends Ob

  sealed trait ObTag
  case object OTptr extends ObTag
  case object OTsym extends ObTag
  case object OTfixnum extends ObTag
  case object OTesc extends ObTag
  case object OTbool extends ObTag
  case object OTchar extends ObTag
  case object OTniv extends ObTag
  case object OTsysval extends ObTag
  case object OTlocation extends ObTag
}
