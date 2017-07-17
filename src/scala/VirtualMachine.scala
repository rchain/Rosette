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
    var opcode : Op;
    var args : List[Int]; // Covers 16-bit args; may want 64-bit ones eventually
}

class Location {}
object Location {
    def ArgReg(a : Int) : Location = {}
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
    var ctxt : Ctxt;
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
                // unwind if u;
                // invoke trgt with m args and tag = litvec[v]
                // nxt if n;
                case OpXmitTag(u : Boolean, n : Boolean, m : Int, v : Int) => {
                    ctxt.nargs = m;
                    ctxt.tag.atom = code.lit(v);
                    doXmitFlag = true;
                }

                case OpXmitArg(u : Boolean, n : Boolean, m : Int, a : Int) => {
                    ctxt.nargs = m;
                    ctxt.tag = ArgReg(a);
                    doXmitFlag = true;
                }

                case OpXmitReg(u : Boolean, n : Boolean, m : Int, r : Int) => {
                    ctxt.nargs = m;
                    ctxt.tag = CtxtReg(r);
                    doXmitFlag = true;
                }

                case OpXmit(u : Boolean, n : Boolean, m : Int) => {
                    doXmitFlag = true;
                }

                case OpXmitTagXtnd(u : Boolean, n : Boolean, m : Int, v : Int) => {
                    doXmitFlag = true;
                }

                case OpXmitArgXtnd(u : Boolean, n : Boolean, m : Int, a : Int) => {
                    doXmitFlag = true;
                }

                case OpXmitRegXtnd(u : Boolean, n : Boolean, m : Int, r : Int) => {
                    doXmitFlag = true;
                }

                case OpSend(u : Boolean, n : Boolean, m : Int) => {
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
                    doRtnFlag = true;
                }

                case OpRtn(n : Boolean) => {
                    doRtnFlag = true;
                }

                case OpRtnArg(n : Boolean, a : Int) => {
                    doRtnFlag = true;
                }

                case OpRtnReg(n : Boolean, r : Int) => {
                    doRtnFlag = true;
                }

                case OpUpcallRtn(n : Boolean, v : Int) => {
                    // may set doNextThreadFlag, vmErrorFlag
                }

                case OpUpcallResume() => {
                }

                case OpNxt() {
                    doNextThreadFlag = true;
                }

                case OpJmpCut(n : Boolean, m : Int) => {
                }

                case OpJmp(n : Boolean) => {
                }

                case OpJmpFalse(n : Boolean) => {
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
                // may set doNextThreadFlag, vmErrorFlag
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