package coop.rchain.rosette

sealed trait LookupError
case object Absent extends LookupError
case object Upcall extends LookupError

class Ob {
  def is(value: Ob.ObTag): Boolean = true

  def lookupOBO(meta: Ob, ob: Ob, key: Ob): Either[LookupError, Ob] =
    Right(Ob.PLACEHOLDER)
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
