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
                case opHalt => {
                    warning("halting...");
                    done = true;
                }

                case opPush => ctxt = Ctxt.create(NIL, ctxt);

                case opPop => ctxt = ctxt.ctxt;

                case opNargs => ctxt.nargs = instr.args[0];

                case opAlloc => ctxt.argvec = Tuple.create(instr.args[0], NIV);

                case opPushAlloc => 
                    ctxt = Ctxt.create(Tuple.create(instr.args[0], NIV), ctxt);

                case opExtend => {
                    
                }
                
                case opOutstanding => {
                    ctxt.pc = PC.fromInt(instr.args[0]);
                    ctxt.outstanding = instr.args[1];
                }
                
                case opFork => {
                    var newCtxt = ctxt.clone();
                    newCtxt.pc = PC.fromInt(instr.args[0]);
                    strandPool.push(newCtxt);
                }
                
                
            }
        } while (!done);
    }
}