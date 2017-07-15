class Instr {
    var opcode;
    var args : List[Int]; // Covers 16-bit args; may want 64-bit ones eventually
}

class PC {
    def fetch() : Instr = {}
}

class Ctxt {
    var ctxt : Option[Ctxt] = None;
    var nargs : Int;
    def create(a, ctxt : Ctxt) : Ctxt = {}
}

class VirtualMachine {
    var sigvec : Int = 0;
    var debuggingLevel : Int = 0;
    var pc : PC;
    var bytecodes : List[Int];
    var ctxt : Ctxt;

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

                case 
            }
        } while (!done);
    }
}