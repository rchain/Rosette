class Code extends Ob {
    def lit(l : Int) : Option[Ob] = {}
}

class Ctxt extends Ob {
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

object Ctxt extends Ob {
    def create(a, ctxt : Ctxt) : Option[Ctxt] = {}
}

class Env extends Ob {
    def parent() : Option[Env] = {}
}

class Instr extends Ob {
    var opcode : Op;
    var args : List[Int]; // Covers 16-bit args; may want 64-bit ones eventually
}

class Location extends Ob {
    var atom : Option[Ob];
}
object Location {
    def ArgReg(a : Int) : Option[Location] = {}
    def CtxtReg(r : Int) : Option[Location] = {}
}

class Ob {}

class PC extends Ob {
    def fetch() : Instr = {}
}

object PC {
    def fromInt(i : Int) : PC = {}    
}

class Prim extends Ob {}

object Prim {
    def nthPrim(n : Int) : Option[Prim] = {}
}

class Tuple extends Ob {}

object Tuple {
    def create(a, b : Option[?]) : Tuple = {}
}

class VMState {
    var bytecodes : List[Int];
    var code : Option[Code];
    var ctxt : Option[Ctxt];
    var debuggingLevel : Int = 0;
    var loc : Location;
    var pc : PC;
    var sigvec : Int = 0;
    var strandPool : scala.collection.mutable.Stack[Ctxt];

    var nextOpFlag : Boolean = true;
    var doXmitFlag : Boolean = false;
    var xmitData : (Boolean, Boolean) = (false, false);
    var doRtnFlag : Boolean = false;
    var doNextThread : Boolean = false;
    var vmErrorFlag : Boolean = false;
    var exitFlag : Boolean = false;
}

trait VirtualMachine {

    def executeStream(
        opCodes : Stream[Op],
        state : VMState
    ) : Unit {
        breakable {
            for (op <- opCodes) {
                executeDispatch(op, state);
                if (state.doXmitFlag) {
                    // may set doNextThreadFlag
                }
                if (state.doRtnFlag) {
                    if (state.ctxt.get.ret(state.ctxt.get.rslt)) {
                        state.vmErrorFlag = true;
                    } else if (state.doRtnData) {
                        state.doNextThreadFlag = true;
                    }
                }
                if (vmErrorFlag) {
                    handleVirtualMachineError();
                    state.doNextThreadFlag = true;
                }
                if (doNextThreadFlag) {
                    if (getNextStrand()) {
                        state.nextOpFlag = false;
                    }
                }
                if (state.exitFlag) {
                    break;
                }
            }
        }
    }

    def executeDispatch(op : Op, state: VMState) {
        match op {
            case o : OpHalt => execute(o, state);
            case o : OpPush => execute(o, state);
            case o : OpPop => execute(o, state);
            case o : OpNargs => execute(o, state);
            case o : OpAlloc => execute(o, state);
            case o : OpPushAlloc => execute(o, state);
            case o : OpExtend => execute(o, state);
            case o : OpOutstanding => execute(o, state);
            case o : OpFork => execute(o, state);
            case o : OpXmitTag => execute(o, state);
            case o : OpXmitArg => execute(o, state);
            case o : OpXmitReg => execute(o, state);
            case o : OpXmit => execute(o, state);
            case o : OpXmitTagXtnd => execute(o, state);
            case o : OpXmitArgXtnd => execute(o, state);
            case o : OpXmitRegXtnd => execute(o, state);
            case o : OpSend => execute(o, state);
            case o : OpApplyPrimTag => execute(o, state);
            case o : OpApplyPrimArg => execute(o, state);
            case o : OpApplyPrimReg => execute(o, state);
            case o : OpApplyCmd => execute(o, state);
            case o : OpRtnTag => execute(o, state);
            case o : OpRtnArg => execute(o, state);
            case o : OpRtnReg => execute(o, state);
            case o : OpRtn => execute(o, state);
            case o : OpUpcallRtn => execute(o, state);
            case o : OpResume => execute(o, state);
            case o : OpNxt => execute(o, state);
            case o : OpJmp => execute(o, state);
            case o : OpJmpFalse => execute(o, state);
            case o : OpJmpCut => execute(o, state);
            case o : OpLookupToArg => execute(o, state);
            case o : OpLookupToReg => execute(o, state);
            case o : OpXferLexToArg => execute(o, state);
            case o : OpXferLexToReg => execute(o, state);
            case o : OpXferGlobalToArg => execute(o, state);
            case o : OpXferGlobalToReg => execute(o, state);
            case o : OpXferArgToArg => execute(o, state);
            case o : OpXferRsltToArg => execute(o, state);
            case o : OpXferArgToRslt => execute(o, state);
            case o : OpXferRsltToReg => execute(o, state);
            case o : OpXferRegToRslt => execute(o, state);
            case o : OpXferRsltToDest => execute(o, state);
            case o : OpXferSrcToRslt => execute(o, state);
            case o : OpIndLitToArg => execute(o, state);
            case o : OpIndLitToReg => execute(o, state);
            case o : OpIndLitToRslt => execute(o, state);
            case o : OpImmediateLitToArg => execute(o, state);
            case o : OpImmediateLitToReg => execute(o, state);
            case o : OpUnknown => execute(o, state);
        }
    }

    def execute(op : OpHalt, state : VMState) = {
        state.exitFlag = true;
    }
    
    def execute(op : OpPush, state : VMState) = {
        state.ctxt = Ctxt.create(None, state.ctxt);        
    }
    
    def execute(op : OpPop, state : VMState) = {
        state.ctxt = state.ctxt.get.ctxt;
    }
    
    def execute(op : OpNargs, state : VMState) = {
        state.ctxt.get.nargs = op.n;
    }
    
    def execute(op : OpNargs, state : VMState) = {
        state.ctxt.get.argvec = Tuple.create(op.n, None);
    }
    
    def execute(op : OpPushAlloc, state : VMState) = {
        state.ctxt = Ctxt.create(Tuple.create(op.n, None), state.ctxt);
    }
    
    def execute(op : OpExtend, state : VMState) = {
        // stuff w/ op.v
    }
    
    def execute(op : OpOutstanding, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.pc = PC.fromInt(op.p);
        ctxt.outstanding = op.n;
    }

    def execute(op : OpFork, state : VMState) = {
        var newCtxt = state.ctxt.get.clone();
        newCtxt.pc = PC.fromInt(op.p);
        state.strandPool.push(newCtxt);
    }

    def execute(op : OpXmitTag, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.nargs = op.m;
        ctxt.tag.atom = state.code.get.lit(op.v);
        state.xmitData = (op.u, op.n);
        state.doXmitFlag = true;
    }

    def execute(op : OpXmitArg, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.nargs = op.m;
        ctxt.tag = ArgReg(op.a);
        state.xmitData = (op.u, op.n);
        state.doXmitFlag = true;
    }

    def execute(op : OpXmitReg, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.nargs = op.m;
        ctxt.tag = CtxtReg(op.r);
        state.xmitData = (op.u, op.n);
        state.doXmitFlag = true;
    }

    def execute(op : OpXmit, state : VMState) = {
        state.ctxt.get.nargs = op.m;
        state.xmitData = (op.u, op.n);
        state.doXmitFlag = true;
    }

    def execute(op : OpXmitTagXtnd, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.nargs = op.m;
        ctxt.tag.atom = state.code.get.lit(op.v);
        state.xmitData = (op.u, op.n);
        state.doXmitFlag = true;
    }

    def execute(op : OpXmitArgXtnd, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.nargs = op.m;
        ctxt.tag = ArgReg(op.a);
        state.xmitData = (op.u, op.n);
        state.doXmitFlag = true;
    }

    def execute(op : OpXmitReg, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.nargs = op.m;
        ctxt.tag = CtxtReg(op.r);
        state.xmitData = (op.u, op.n);
        state.doXmitFlag = true;
    }

    def execute(op : OpSend, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.ctxt = None;
        ctxt.nargs = op.m;
        state.xmitData = (op.u, op.n);
        state.doXmitFlag = true;
    }

    def execute(op : OpApplyPrimTag, state : VMState) = {
        val ctxt = state.ctxt.get;
        ctxt.nargs = op.m;
        val prim = Prim.nthPrim(op.k);
        state.loc.atom = state.code.get.lit(op.v);
        val result = if { (u) unwindAndApply(prim) }
                     else { prim.get.dispatchHelper(state.ctxt) };
        if (result == DEADTHREAD) {
            state.doNextThreadFlag = true;
        } else if (result.is(OTsysval)) {
            handleException(result, op, state.loc);
            state.doNextThreadFlag = true;
        } else if (store(state.loc, state.ctxt, result)) {
            state.vmErrorFlag = true;
        } else if (op.n) {
            state.doNextThreadFlag = true;
        }
    }

    def execute(op : OpApplyPrimArg, state : VMState) = {
        // unmka
        // may set doNextThreadFlag, vmErrorFlag
    }

    def execute(op : OpApplyPrimReg, state : VMState) = {
        // unmkr
        // may set doNextThreadFlag, vmErrorFlag
    }

    def execute(op : OpApplyCmd, state : VMState) = {
        // unmk
        // may set doNextThreadFlag, vmErrorFlag
    }

    def execute(op : OpRtn, state : VMState) = {
        state.doRtnData = op.n;
        state.doRtnFlag = true;
    }

    def execute(op : OpRtnTag, state : VMState) = {
        state.ctxt.get.tag.atom = op.v;
        state.doRtnData = op.n;
        state.doRtnFlag = true;
    }

    def execute(op : OpRtnArg, state : VMState) = {
        state.ctxt.get.tag = ArgReg(op.a);
        state.doRtnData = op.n;
        state.doRtnFlag = true;
    }

    def execute(op : OpRtnReg, state : VMState) = {
        state.ctxt.get.tag = CtxtReg(op.r);
        state.doRtnData = op.n;
        state.doRtnFlag = true;
    }

    def execute(op : OpRtnReg, state : VMState) = {
        val optCtxt = state.ctxt.get;
        optCtxt.tag.atom = state.code.get.lit(op.v);
        if (optCtxt.tag.store(optCtxt.ctxt, optCtxt.rslt)) {
            state.vmErrorFlag = true;
        } else if (n) {
            state.doNextThreadFlag = true;
        }
    }

    def execute(op : OpRtnReg, state : VMState) = {
        state.ctxt.get.ctxt.get.scheduleStrand();
    }

    def execute(op : OpJmpCut, state : VMState) = {
        state.doNextThreadFlag = true;
    }

    def execute(op : OpJmp, state : VMState) = {
        state.pc.absolute = code.get.absolutize(op.n);
    }

    def execute(op : OpJmpCut, state : VMState) = {
        var cut = op.m;
        var newEnv : Option[Env] = state.ctxt.get.parent();
        while(0 < cut) {
            newEnv = newEnv.get.parent();
            cut -= 1;
        }
        state.ctxt.get.env = newEnv;
        state.pc.absolute = state.code.get.absolutize(op.n);
    }

    def execute(op : OpJmpFalse, state : VMState) = {
        if (state.ctxt.get.rslt == false) {
            state.pc.absolute = state.code.get.absolutize(op.n);
        }
    }

    def execute(op : OpLookupToArg, state : VMState) = {
        val argno = op.a;
        val key : Option[Ob] = Some(state.code.get.lit(op.v));
        // may set doNextThreadFlag
    }

    def execute(op : OpLookupToReg, state : VMState) = {
        // rv
        // may set doNextThreadFlag
    }

    def execute(op : OpLookupToArg, state : VMState) = {
        // av
        // may set doNextThreadFlag
    }

    execute(op : OpXferLexToArg, state : VMState) = {
        // i : Boolean, l : Int, o : Int, a : Int

    }

    execute(op : OpXferLexToReg, state : VMState) = {
        // i : Boolean, l : Int, o : Int, r : Int

    }

    execute(op : OpXferGlobalToArg, state : VMState) = {
        // a : Int, g : Int

    }

    execute(op : OpXferGlobalToReg, state : VMState) = {
        // r : Int, g : Int

    }

    execute(op : OpXferArgToArg, state : VMState) = {
        // d : Int, s : Int

    }

    execute(op : OpXferRsltToArg, state : VMState) = {
        // a : Int

    }

    execute(op : OpXferArgToRslt, state : VMState) = {
        // a : Int

    }

    execute(op : OpXferRsltToReg, state : VMState) = {
        // r : Int

    }

    execute(op : OpXferRegToRslt, state : VMState) = {
        // r : Int

    }

    execute(op : OpXferRsltToDest, state : VMState) = {
        // v : Int
            // may set vmErrorFlag

    }

    execute(op : OpXferSrcToRslt, state : VMState) = {
        // v : Int

    }

    execute(op : OpIndLitToArg, state : VMState) = {
        // a : Int, v : Int

    }

    execute(op : OpIndLitToReg, state : VMState) = {
        // r : Int, v : Int

    }

    execute(op : OpIndLitToRslt, state : VMState) = {
        // v : Int

    }

    execute(op : OpImmediateLitToArg, state : VMState) = {
        // fixnum: Boolean, v : Int, a : Int

    }

    execute(op : OpImmediateLitToReg, state : VMState) = {
        // fixnum: Boolean, v : Int, r : Int

    }

    execute(op : OpUnknown, state : VMState) = {
        // 
        doNextThreadFlag = true;
    }
}