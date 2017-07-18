class Ctxt {
    var argvec : Tuple;
    var ctxt : Option[Ctxt] = None;
    var nargs : Int;
    var outstanding : Int;
    var tag : Location;
    var rslt;
    
    def parent() : Option[Env] = {}
    def ret(rslt) : Boolean = {}
    def scheduleStrand() : Unit = {}
}

object Ctxt {
    def create(a, ctxt : Ctxt) : Option[Ctxt] = {}
}

class Env {
    def parent() : Option[Env] = {}
}

class Instr {
    var opcode : Op;
    var args : List[Int]; // Covers 16-bit args; may want 64-bit ones eventually
}

class Location {}
object Location {
    def ArgReg(a : Int) : Option[Location] = {}
    def CtxtReg(r : Int) : Option[Location] = {}
}

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
    var code : Option[Code];
    var ctxt : Option[Ctxt];
    var debuggingLevel : Int = 0;
    var pc : PC;
    var sigvec : Int = 0;
    var strandPool : scala.collection.mutable.Stack[Ctxt];

    def execute() : Unit = {
        var instr : Instr;
        var loc : Location;
        var temp : Option[Ob] = None;
        var result : Option[Ob] = None;

        do {
            var nextOpFlag : Boolean = true;
            var doXmitFlag : Boolean = false;
            var xmitData : (Boolean, Boolean) = (false, false);
            var doRtnFlag : Boolean = false;
            var doNextThread : Boolean = false;
            var vmErrorFlag : Boolean = false;
            var exitFlag : Boolean = false;

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

                case OpPop() => ctxt = ctxt.get.ctxt;

                case OpNargs(n) => ctxt.get.nargs = n;

                case OpAlloc(n) => ctxt.get.argvec = Tuple.create(n, NIV);

                case OpPushAlloc(n) => 
                    ctxt = Ctxt.create(Tuple.create(n, NIV), ctxt);

                case OpExtend(v) => {
                    
                }
                
                case OpOutstanding(p, n) => {
                    ctxt.get.pc = PC.fromInt(p);
                    ctxt.get.outstanding = n;
                }
                
                case OpFork(p) => {
                    var newCtxt = ctxt.get.clone();
                    newCtxt.pc = PC.fromInt(p);
                    strandPool.push(newCtxt);
                }
                case OpXmitTag(u : Boolean, n : Boolean, m : Int, v : Int) => {
                    ctxt.get.nargs = m;
                    ctxt.get.tag.atom = code.get.lit(v);
                    xmitData = (u, n);
                    doXmitFlag = true;
                }

                case OpXmitArg(u : Boolean, n : Boolean, m : Int, a : Int) => {
                    ctxt.get.nargs = m;
                    ctxt.get.tag = ArgReg(a);
                    xmitData = (u, n);
                    doXmitFlag = true;
                }

                case OpXmitReg(u : Boolean, n : Boolean, m : Int, r : Int) => {
                    ctxt.get.nargs = m;
                    ctxt.get.tag = CtxtReg(r);
                    xmitData = (u, n);
                    doXmitFlag = true;
                }

                case OpXmit(u : Boolean, n : Boolean, m : Int) => {
                    ctxt.get.nargs = m;
                    xmitData = (u, n);
                    doXmitFlag = true;
                }

                case OpXmitTagXtnd(u : Boolean, n : Boolean, m : Int, v : Int) => {
                    ctxt.get.nargs = m;
                    ctxt.get.tag.atom = code.get.lit(v);
                    xmitData = (u, n);
                    doXmitFlag = true;
                }

                case OpXmitArgXtnd(u : Boolean, n : Boolean, m : Int, a : Int) => {
                    ctxt.get.nargs = m;
                    ctxt.get.tag = ArgReg(a);
                    xmitData = (u, n);
                    doXmitFlag = true;
                }

                case OpXmitRegXtnd(u : Boolean, n : Boolean, m : Int, r : Int) => {
                    ctxt.get.nargs = m;
                    ctxt.get.tag = CtxtReg(r);
                    xmitData = (u, n);
                    doXmitFlag = true;
                }

                case OpSend(u : Boolean, n : Boolean, m : Int) => {
                    ctxt.get.ctxt = None;
                    ctxt.get.nargs = m;
                    xmitData = (u, n);
                    doXmitFlag = true;
                }

                case OpApplyPrimTag(u : Boolean, n : Boolean, m : Int, k : Int, v : Int) => {
                    // may set doNextThreadFlag, vmErrorFlag
                }

                case OpApplyPrimArg(u : Boolean, n : Boolean, m : Int, k : Int, a : Int) => {
                    // may set doNextThreadFlag, vmErrorFlag
                }

                case OpApplyPrimReg(u : Boolean, n : Boolean, m : Int, k : Int, r : Int) => {
                    // may set doNextThreadFlag, vmErrorFlag
                }

                case OpApplyCmd(u : Boolean, n : Boolean, m : Int, k : Int) => {
                    // may set doNextThreadFlag, vmErrorFlag
                }

                case OpRtnTag(n : Boolean, v : Int) => {
                    doRtnData = n;
                    doRtnFlag = true;
                }

                case OpRtn(n : Boolean) => {
                    doRtnData = n;
                    doRtnFlag = true;
                }

                case OpRtnArg(n : Boolean, a : Int) => {
                    doRtnData = n;
                    doRtnFlag = true;
                }

                case OpRtnReg(n : Boolean, r : Int) => {
                    doRtnData = n;
                    doRtnFlag = true;
                }

                case OpUpcallRtn(n : Boolean, v : Int) => {
                    val optCtxt = ctxt.get;
                    optCtxt.tag.atom = code.get.lit(v);
                    if (optCtxt.tag.store(optCtxt.ctxt, optCtxt.rslt)) {
                        vmErrorFlag = true;
                    } else if (n) {
                        doNextThreadFlag = true;
                    }
                }

                case OpUpcallResume() => {
                    ctxt.get.ctxt.get.scheduleStrand();
                }

                case OpNxt() {
                    doNextThreadFlag = true;
                }

                case OpJmpCut(n : Boolean, m : Int) => {
                    var cut = m;
                    var newEnv : Option[Env] = ctxt.get.parent();
                    while(0 < cut) {
                        newEnv = newEnv.get.parent();
                        cut -= 1;
                    }
                    ctxt.get.env = newEnv;
                }

                case OpJmp(n : Boolean) => {
                    pc.absolute = code.get.absolutize(n);
                }

                case OpJmpFalse(n : Boolean) => {
                    if (ctxt.get.rslt == false) {
                        pc.absolute = code.get.absolutize(n);
                    }
                }

                case OpLookupToArg(a : Int, v : Int) => {
                    // may set doNextThreadFlag
                }

                case OpLookupToReg(r : Int, v : Int) => {
                    // may set doNextThreadFlag
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

                case OpXferRsltToDest(v : Int) => {
                    // may set vmErrorFlag
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
                    doNextThreadFlag = true;
                }
            }
            if (doXmitFlag) {
                // may set doNextThreadFlag
            }
            if (doRtnFlag) {
                if (ctxt.get.ret(ctxt.get.rslt)) {
                    vmErrorFlag = true;
                } else if (doRtnData) {
                    doNextThreadFlag = true;
                }
            }
            if (vmErrorFlag) {
                handleVirtualMachineError();
                doNextThreadFlag = true;
            }
            if (doNextThreadFlag) {
                if (getNextStrand()) {
                    nextOpFlag = false;
                }
            }
        } while (nextOpFlag);
    }
}