class Ctxt {
    var argvec : Tuple;
    var ctxt : Option[Ctxt] = None;
    var nargs : Int;
    var outstanding : Int;
}

object Ctxt {
    def create(a, ctxt : Ctxt) : Ctxt = {}
}

class Instr {
    var opcode;
    var args : List[Int]; // Covers 16-bit args; may want 64-bit ones eventually
}

abstract class Op;
case class OpHalt() extends Op;
case class OpPush() extends Op;
case class OpPop() extends Op;
case class OpNargs(n : Int) extends Op;
case class OpAlloc(n : Int) extends Op;
case class OpPushAlloc(n : Int) extends Op;
case class OpExtend(v : Int) extends Op;
case class OpOutstanding(p : Int, n : Int) extends Op;
case class OpFork(p : Int) extends Op;
case class OpXmitTag(n : Boolean, u : Boolean, m : Int, v : Int) extends Op;
case class OpXmitArg(u : Boolean, n : Boolean, m : Int, a : Int) extends Op;
case class OpXmitReg(u : Boolean, n : Boolean, m : Int, r : Int) extends Op;
case class OpXmit(u : Boolean, n : Boolean, m : Int) extends Op;
case class OpXmitTagXtnd(u : Boolean, n : Boolean, m : Int, v : Int) extends Op;
case class OpXmitArgXtnd(u : Boolean, n : Boolean, m : Int, a : Int) extends Op;
case class OpXmitRegXtnd(u : Boolean, n : Boolean, m : Int, r : Int) extends Op;
case class OpSend(u : Boolean, n : Boolean, m : Int) extends Op;
case class OpApplyPrimTag(u : Boolean, n : Boolean, m : Int, k : Int, v : Int) extends Op;
case class OpApplyPrimArg(u : Boolean, n : Boolean, m : Int, k : Int, a : Int) extends Op;
case class OpApplyPrimReg(u : Boolean, n : Boolean, m : Int, k : Int, r : Int) extends Op;
case class OpApplyCmd(u : Boolean, n : Boolean, m : Int, k : Int) extends Op;
case class OpRtnTag(n : Boolean, v : Int) extends Op;
case class OpRtnArg(n : Boolean, a : Int) extends Op;
case class OpRtnReg(n : Boolean, r : Int) extends Op;
case class OpRtn(n : Boolean) extends Op;
case class OpUpcallRtn(n : Boolean, v : Int) extends Op;
case class OpResume() extends Op;
case class OpNxt() extends Op;
case class OpJmp(n : Boolean) extends Op;
case class OpJmpFalse(n : Boolean) extends Op;
case class OpJmpCut(n : Boolean, m : Int) extends Op;
case class OpLookupToArg(a : Int, v : Int) extends Op;
case class OpLookupToReg(r : Int, v : Int) extends Op;
case class OpXferLexToArg(i : Boolean, l : Int, o : Int, a : Int) extends Op;
case class OpXferLexToReg(i : Boolean, l : Int, o : Int, r : Int) extends Op;
case class OpXferGlobalToArg(a : Int, g : Int) extends Op;
case class OpXferGlobalToReg(r : Int, g : Int) extends Op;
case class OpXferArgToArg(d : Int, s : Int) extends Op;
case class OpXferRsltToArg(a : Int) extends Op;
case class OpXferArgToRslt(a : Int) extends Op;
case class OpXferRsltToReg(r : Int) extends Op;
case class OpXferRegToRslt(r : Int) extends Op;
case class OpXferSrcToRslt(v : Int) extends Op;
case class OpIndLitToArg(a : Int, v : Int) extends Op;
case class OpIndLitToReg(r : Int, v : Int) extends Op;
case class OpIndLitToRslt(v : Int) extends Op;
case class OpImmediateLitToArg(fixnum: Boolean, v : Int, a : Int) extends Op;
case class OpImmediateLitToReg(fixnum: Boolean, v : Int, r : Int) extends Op;
case class OpUnknown() extends Op;

class PC {
    def fetch() : Instr = {}
}

object PC {
    def fromInt(i : Int) : PC = {}    
}

class Tuple {
    
}

object Tuple {
    def create(a, b) : Tuple = {}
}

class VirtualMachine {
    var bytecodes : List[Int];
    var ctxt : Ctxt;
    var debuggingLevel : Int = 0;
    var pc : PC;
    var sigvec : Int = 0;
    var strandPool : scala.collection.mutable.Stack[Ctxt];

    def execute() : Unit = {
        var instr : Instr;
        var loc : Location;
        var temp : Ob = INVALID;
        var result : Ob = INVALID;
        var done : Boolean = false;

        do {
            if (0 != sigvec) {
                // handleSignal();
            }

            if (0 < debuggingLevel) {
                // traceOpCode();
            }

            instr = pc.fetch();
            bytecodes[instr.opcode] += 1;

            instr.opcode match {
                case OpHalt() => {
                    warning("halting...");
                    done = true;
                }

                case OpPush() => ctxt = Ctxt.create(NIL, ctxt);

                case OpPop() => ctxt = ctxt.ctxt;

                case OpNargs(n) => ctxt.nargs = n;

                case OpAlloc(n) => ctxt.argvec = Tuple.create(n, NIV);

                case OpPushAlloc(n) => 
                    ctxt = Ctxt.create(Tuple.create(n, NIV), ctxt);

                case OpExtend(v) => {
                    
                }
                
                case OpOutstanding(p, n) => {
                    ctxt.pc = PC.fromInt(p);
                    ctxt.outstanding = n;
                }
                
                case OpFork(p) => {
                    var newCtxt = ctxt.clone();
                    newCtxt.pc = PC.fromInt(p);
                    strandPool.push(newCtxt);
                }
                
                case OpXmitTag(n : Boolean, u : Boolean, m : Int, v : Int) => {
                }
                case OpXmitArg(u : Boolean, n : Boolean, m : Int, a : Int) => {
                }
                case OpXmitReg(u : Boolean, n : Boolean, m : Int, r : Int) => {
                }
                case OpXmit(u : Boolean, n : Boolean, m : Int) => {
                }
                case OpXmitTagXtnd(u : Boolean, n : Boolean, m : Int, v : Int) => {
                }
                case OpXmitArgXtnd(u : Boolean, n : Boolean, m : Int, a : Int) => {
                }
                case OpXmitRegXtnd(u : Boolean, n : Boolean, m : Int, r : Int) => {
                }
                case OpSend(u : Boolean, n : Boolean, m : Int) => {
                }
                case OpApplyPrimTag(u : Boolean, n : Boolean, m : Int, k : Int, v : Int) => {
                }
                case OpApplyPrimArg(u : Boolean, n : Boolean, m : Int, k : Int, a : Int) => {
                }
                case OpApplyPrimReg(u : Boolean, n : Boolean, m : Int, k : Int, r : Int) => {
                }
                case OpApplyCmd(u : Boolean, n : Boolean, m : Int, k : Int) => {
                }
                case OpRtnTag(n : Boolean, v : Int) => {
                }
                case OpRtnArg(n : Boolean, a : Int) => {
                }
                case OpRtnReg(n : Boolean, r : Int) => {
                }
                case OpRtn(n : Boolean) => {
                }
                case OpUpcallRtn(n : Boolean, v : Int) => {
                }
                case OpResume() => {
                }
                case OpNxt() => {
                }
                case OpJmp(n : Boolean) => {
                }
                case OpJmpFalse(n : Boolean) => {
                }
                case OpJmpCut(n : Boolean, m : Int) => {
                }
                case OpLookupToArg(a : Int, v : Int) => {
                }
                case OpLookupToReg(r : Int, v : Int) => {
                }
                case OpXferLexToArg(i : Boolean, l : Int, o : Int, a : Int) => {
                }
                case OpXferLexToReg(i : Boolean, l : Int, o : Int, r : Int) => {
                }
                case OpXferGlobalToArg(a : Int, g : Int) => {
                }
                case OpXferGlobalToReg(r : Int, g : Int) => {
                }
                case OpXferArgToArg(d : Int, s : Int) => {
                }
                case OpXferRsltToArg(a : Int) => {
                }
                case OpXferArgToRslt(a : Int) => {
                }
                case OpXferRsltToReg(r : Int) => {
                }
                case OpXferRegToRslt(r : Int) => {
                }
                case OpXferSrcToRslt(v : Int) => {
                }
                case OpIndLitToArg(a : Int, v : Int) => {
                }
                case OpIndLitToReg(r : Int, v : Int) => {
                }
                case OpIndLitToRslt(v : Int) => {
                }
                case OpImmediateLitToArg(fixnum: Boolean, v : Int, a : Int) => {
                }
                case OpImmediateLitToReg(fixnum: Boolean, v : Int, r : Int) => {
                }
                case OpUnknown() => {
                }
                
            }
        } while (!done);
    }
}