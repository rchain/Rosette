package coop.rchain.rosette

case class Location(atom: Ob,
                    genericType: Location.GenericType,
                    override val entry: Seq[Ob],
                    override val meta: Ob,
                    override val slot: Seq[Ob])
    extends Ob

object Location {
  import Ob.Lenses._

  object PLACEHOLDER extends Location(null, LTLimbo, null, null, null)
  object LIMBO extends Location(null, LTLimbo, null, null, null)

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
  case object LTLimbo extends GenericType

  def ArgReg(a: Int): Location = PLACEHOLDER
  def CtxtReg(r: Int): Location = PLACEHOLDER
  def fetch(loc: Location, k: Ctxt, globalEnv: TblObject): Ob = null
  def isFixNum(value: Ob): Boolean = false
  def fixVal(value: Ob): Int = 0

  sealed trait StoreResult
  case object StoreFail extends StoreResult
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
        if (argReg < k.argvec.elem.size) {
          StoreCtxt(k.update(_ >> 'argvec >> 'elem)(_.updated(argReg, value)))
        } else {
          StoreFail
        }

      case LTLexVariable(ind, level, offset) =>
        k.env.setLex(ind, level, offset, value) match {
          case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
          case None => StoreFail
        }

      case LTAddrVariable(ind, level, offset) =>
        k.env.setLex(ind, level, offset, value) match {
          case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
          case None => StoreFail
        }

      case LTGlobalVariable(offset) =>
        if (offset < globalEnv.numberOfSlots()) {
          StoreGlobal(globalEnv.update(_ >> 'slot)(_.updated(offset, value)))
        } else {
          StoreFail
        }

      case LTBitField(ind, level, offset, spanSize) =>
        if (!isFixNum(value)) {
          k.env.setField(ind, level, offset, spanSize, fixVal(value)) match {
            case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
            case None => StoreFail
          }
        } else {
          StoreFail
        }

      case LTBitField00(offset, spanSize) =>
        if (!isFixNum(value)) {
          k.env.setField(0, 0, offset, spanSize, fixVal(value)) match {
            case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
            case None => StoreFail
          }
        } else {
          StoreFail
        }

      case LTLimbo => StoreFail
    }
}
