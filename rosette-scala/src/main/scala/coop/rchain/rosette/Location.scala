package coop.rchain.rosette

case class Location(atom: Ob,
                    genericType: Location.GenericType,
                    override val meta: Ob,
                    override val slot: Seq[Ob])
    extends Ob

object Location {
  import Ob.Lenses._

  object PLACEHOLDER extends Location(null, LTLimbo, null, null)
  object LIMBO extends Location(null, LTLimbo, null, null)

  sealed trait GenericType
  case class LTCtxtRegister(reg: Int) extends GenericType
  case class LTArgRegister(argReg: Int) extends GenericType
  case class LTLexVariable(indirect: Int, level: Int, offset: Int)
      extends GenericType
  case class LTAddrVariable(indirect: Int, level: Int, offset: Int)
      extends GenericType
  case class LTGlobalVariable(offset: Int) extends GenericType
  case class LTBitField(indirect: Int,
                        level: Int,
                        offset: Int,
                        spanSize: Int,
                        sign: Int)
      extends GenericType
  case class LTBitField00(offset: Int, spanSize: Int, sign: Int)
      extends GenericType
  case object LTLimbo extends GenericType

  def ArgReg(a: Int): Location = PLACEHOLDER
  def CtxtReg(r: Int): Location = PLACEHOLDER
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

      case LTLexVariable(indirect, level, offset) =>
        k.env.setLex(indirect, level, offset, value) match {
          case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
          case None => StoreFail
        }

      case LTAddrVariable(indirect, level, offset) =>
        k.env.setAddr(indirect, level, offset, value) match {
          case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
          case None => StoreFail
        }

      case LTGlobalVariable(offset) =>
        if (offset < globalEnv.numberOfSlots()) {
          StoreGlobal(globalEnv.update(_ >> 'slot)(_.updated(offset, value)))
        } else {
          StoreFail
        }

      case LTBitField(indirect, level, offset, spanSize, sign) =>
        if (!isFixNum(value)) {
          k.env.setField(indirect, level, offset, spanSize, fixVal(value)) match {
            case Some(env) => StoreCtxt(k.set(_ >> 'env)(env))
            case None => StoreFail
          }
        } else {
          StoreFail
        }

      case LTBitField00(offset, spanSize, sign) =>
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

  def fetch(loc: Location, k: Ctxt, globalEnv: TblObject): Ob =
    loc.genericType match {
      case LTCtxtRegister(reg) =>
        if (reg < k.reg.size) {
          k.reg(reg)
        } else {
          Ob.INVALID
        }

      case LTArgRegister(argReg) =>
        if (argReg < k.argvec.elem.size) {
          k.argvec.elem(argReg)
        } else {
          Ob.INVALID
        }

      case LTLexVariable(indirect, level, offset) =>
        k.env.getLex(indirect, level, offset)

      case LTAddrVariable(indirect, level, offset) =>
        k.env.getAddr(indirect, level, offset)

      case LTGlobalVariable(offset) =>
        if (offset < globalEnv.numberOfSlots()) {
          globalEnv.slot(offset)
        } else {
          Ob.INVALID
        }

      case LTBitField(indirect, level, offset, spanSize, sign) =>
        k.env.getField(indirect, level, offset, spanSize, sign)

      case LTBitField00(offset, spanSize, sign) =>
        k.env.getField(0, 0, offset, spanSize, sign)

      case LTLimbo => Ob.INVALID
    }
}
