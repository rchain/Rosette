package coop.rchain.rosette

case class Location(atom: Ob,
                    genericType: Location.GenericType,
                    override val meta: Ob,
                    override val slot: Seq[Ob])
    extends Ob

object Location {
  import Ob.Lenses._

  val NumberOfCtxtRegs = 10

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
                        sign: Int = 0)
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
          case env if env != Ob.INVALID => StoreCtxt(k.set(_ >> 'env)(env))
          case _ => StoreFail
        }

      case LTAddrVariable(indirect, level, offset) =>
        k.env.setAddr(indirect, level, offset, value) match {
          case env if env != Ob.INVALID => StoreCtxt(k.set(_ >> 'env)(env))
          case _ => StoreFail
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
            case env if env != Ob.INVALID => StoreCtxt(k.set(_ >> 'env)(env))
            case _ => StoreFail
          }
        } else {
          StoreFail
        }

      case LTBitField00(offset, spanSize, sign) =>
        if (!isFixNum(value)) {
          k.env.setField(0, 0, offset, spanSize, fixVal(value)) match {
            case env if env != Ob.INVALID => StoreCtxt(k.set(_ >> 'env)(env))
            case _ => StoreFail
          }
        } else {
          StoreFail
        }

      case LTLimbo => StoreFail
    }

  def fetch(loc: Location, k: Ctxt, globalEnv: TblObject): Ob =
    loc.genericType match {
      case LTCtxtRegister(reg) =>
        if (reg < Location.NumberOfCtxtRegs) {
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

  def printRep(loc: Location): String = {
    val names = List("rslt",
                     "trgt",
                     "argvec",
                     "env",
                     "code",
                     "ctxt",
                     "self",
                     "self-env",
                     "rcvr",
                     "monitor")

    loc.genericType match {
      case LTCtxtRegister(reg) =>
        if (0 <= reg && reg < Location.NumberOfCtxtRegs) {
          names(reg)
        } else {
          f"unknown ctxt register 0x$reg%x"
        }

      case LTArgRegister(argReg) =>
        s"arg[$argReg]"

      case LTLexVariable(indirect, level, offset) => {
        val offsetStr = if (indirect != 0) s"($offset)" else s"$offset"
        s"lex[$level,$offsetStr]"
      }

      case LTAddrVariable(indirect, level, offset) => {
        val offsetStr = if (indirect != 0) s"($offset)" else s"$offset"
        s"addr[$level,$offsetStr]"
      }

      case LTGlobalVariable(offset) =>
        s"global[$offset]"

      case LTBitField(indirect, level, offset, spanSize, sign) => {
        val signStr = if (sign != 0) "s" else "u"
        val offsetStr = if (indirect != 0) s"($offset)" else s"$offset"
        s"${signStr}fld[$level,$offsetStr,$spanSize]"
      }

      case LTBitField00(offset, spanSize, sign) => {
        val signStr = if (sign != 0) "s" else "u"
        s"${signStr}fld[$offset,$spanSize]"
      }

      case LTLimbo => "limbo"
    }
  }

  def valWRT(loc: Location, v: Ob, globalEnv: TblObject): Ob =
    loc.genericType match {
      case LTLexVariable(indirect, level, offset) =>
        v.getLex(indirect, level, offset)

      case LTAddrVariable(indirect, level, offset) =>
        v.getAddr(indirect, level, offset)

      case LTGlobalVariable(offset) =>
        globalEnv.getLex(1, 0, offset)

      case LTBitField(indirect, level, offset, spanSize, sign) =>
        v.getField(indirect, level, offset, spanSize, sign)

      case LTBitField00(offset, spanSize, sign) =>
        v.getField(0, 0, offset, spanSize, sign)

      case LTLimbo => Ob.ABSENT

      case _ => {
        suicide("valWrt")
        null
      }
    }

  def setValWrt(loc: Location, v: Ob, globalEnv: TblObject, value: Ob): Ob =
    loc.genericType match {
      case LTLexVariable(indirect, level, offset) =>
        v.setLex(indirect, level, offset, value)

      case LTAddrVariable(indirect, level, offset) =>
        v.setAddr(indirect, level, offset, value)

      case LTGlobalVariable(offset) =>
        globalEnv.setLex(1, 0, offset, value)

      case LTBitField(indirect, level, offset, spanSize, sign) =>
        v.setField(indirect, level, offset, spanSize, fixVal(value))

      case LTBitField00(offset, spanSize, sign) =>
        v.setField(0, 0, offset, spanSize, fixVal(value))

      case _ => {
        suicide("setValWrt")
        null
      }
    }
}
