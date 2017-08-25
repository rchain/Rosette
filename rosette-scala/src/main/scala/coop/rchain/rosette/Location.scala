package coop.rchain.rosette

case class Location(atom: Ob, genericType: Location.GenericType) extends Ob {}

object Location {
  import Lenses._

  object PLACEHOLDER extends Location(Ob.PLACEHOLDER, LTLimbo())
  object LIMBO extends Location(Ob.PLACEHOLDER, LTLimbo())

  sealed trait GenericType
  case class LTCtxtRegister(reg: Int) extends GenericType
  case class LTArgRegister(argReg: Int) extends GenericType
  case class LTLexVariable(ind: Int, level: Int, offset: Int)
      extends GenericType
  case class LTAddrVariable(ind: Int, level: Int, offset: Int)
      extends GenericType
  case class LTGlobalVariable(offset: Int) extends GenericType
  case class LTBitField(ind: Int, level: Int, offset: Int, spanSize: Int)
      extends GenericType
  case class LTBitField00(offset: Int, spanSize: Int) extends GenericType
  case class LTLimbo() extends GenericType

  def ArgReg(a: Int): Location = PLACEHOLDER
  def CtxtReg(r: Int): Location = PLACEHOLDER
  def fetch(loc: Location, k: Ctxt): Ob = Ob.PLACEHOLDER
  def isFixNum(value: Ob): Boolean = false
  def fixVal(value: Ob): Int = 0

  sealed trait StoreResult
  case class StoreFail() extends StoreResult
  case class StoreCtxt(ctxt: Ctxt) extends StoreResult
  case class StoreGlobal(globalEnv: TblObject) extends StoreResult

  def store(loc: Location,
            k: Ctxt,
            globalEnv: TblObject,
            value: Ob): StoreResult =
    loc.genericType match {
      case LTCtxtRegister(reg) =>
        StoreCtxt(k.update(_ >> 'reg)(_.updated(reg, value)))

      case LTArgRegister(argReg) =>
        if (argReg > k.argvec.elem.length) {
          StoreFail()
        } else {
          StoreCtxt(k.update(_ >> 'argvec >> 'elem)(_.updated(argReg, value)))
        }

      case LTLexVariable(ind, level, offset) =>
        k.env.setLex(ind, level, offset, value) match {
          case None => StoreFail()
          case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
        }

      case LTAddrVariable(ind, level, offset) =>
        k.env.setLex(ind, level, offset, value) match {
          case None => StoreFail()
          case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
        }

      case LTGlobalVariable(offset) =>
        if (offset > globalEnv.container().numberOfSlots()) {
          StoreFail()
        } else {
          StoreGlobal(globalEnv.update(_ >> 'slot)(_.updated(offset, value)))
        }

      case LTBitField(ind, level, offset, span) =>
        if (isFixNum(value)) {
          StoreFail()
        } else {
          k.env.setField(ind, level, offset, span, fixVal(value)) match {
            case None => StoreFail()
            case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
          }
        }

      case LTBitField00(offset, span) =>
        if (isFixNum(value)) {
          StoreFail()
        } else {
          k.env.setField(0, 0, offset, span, fixVal(value)) match {
            case None => StoreFail()
            case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
          }
        }
      case LTLimbo() => StoreFail()
    }
}
