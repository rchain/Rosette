package coop.rchain.rosette

case class Location(atom: Ob, genericType: Location.GenericType) extends Ob {}

object Location {
  object PLACEHOLDER extends Location(Ob.PLACEHOLDER, Limbo())
  object LIMBO extends Location(Ob.PLACEHOLDER, Limbo())

  sealed trait GenericType
  case class LTCtxtRegister(reg: Int) extends GenericType
  case class LTArgRegister(argReg: Int) extends GenericType
  case class LTLexVariable(ind: Int, level: Int, offset: Int) extends GenericType
  case class LTAddrVariable(ind: Int, level: Int, offset: Int)
      extends GenericType
  case class LTGlobalVariable(offset: Int) extends GenericType
  case class LTBitField(ind: Int, level: Int, offset: Int, span: Int)
      extends GenericType
  case class LTBitField00(offset: Int, span: Int) extends GenericType
  case class LTLimbo() extends GenericType

  def ArgReg(a: Int): Location = PLACEHOLDER
  def CtxtReg(r: Int): Location = PLACEHOLDER
  def fetch(loc: Location, k: Ctxt): Ob = Ob.PLACEHOLDER

  // Returns true if there's an error
  def store(loc: Location, k: Ctxt, value: Ob): Boolean =
    loc.genericType match {
      case LTCtxtRegister(reg) => { true }
      case LTArgRegister(argReg) => { true }
      case LTLexVariable(ind, level, offset) => { true }
      case LTAddrVariable(ind, level, offset) => { true }
      case LTGlobalVariable(offset) => { true }
      case LTBitField(ind, level, offset, span) => { true }
      case LTBitField00(offset, span) => { true }
      case LTLimbo() => { true }
    }
}
