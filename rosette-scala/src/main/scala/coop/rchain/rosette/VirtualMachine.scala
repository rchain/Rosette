package coop.rchain.rosette

trait VirtualMachine {

  def executeSeq(opCodes: Seq[Op], state: VMState): VMState = {
    var pc = 0
    var exit = false
    var currentState = state

    while (pc < opCodes.size && !exit) {
      val op = opCodes(pc)
      currentState = modifyFlags(executeDispatch(op, state))

      pc = currentState.pc.relative

      if (currentState.exitFlag) exit = true
    }

    currentState
  }

  def modifyFlags(state: VMState): VMState = {
    var mState = state

    if (mState.doXmitFlag) {
      // may set doNextThreadFlag
    }

    if (mState.doRtnFlag) {
      //if (mState.ctxt.get.ret(mState.ctxt.get.rslt)) {
      //  mState = mState.set(_ >> 'vmErrorFlag)(true)
      //} else if (mState.doRtnFlag) {
      //  mState = mState.set(_ >> 'doNextThreadFlag)(true)
      //}
    }

    if (mState.vmErrorFlag) {
      //handleVirtualMachineError()
      mState = mState.set(_ >> 'doNextThreadFlag)(true)
    }

    if (mState.doNextThreadFlag) {
      //if (getNextStrand()) {
      //  tmpState = tmpState.set(_ >> 'nextOpFlag)(false)
      //}
    }

    mState
  }

  def executeDispatch(op: Op, state: VMState): VMState =
    op match {
      case o: OpHalt => execute(o, state)
      case o: OpPush => execute(o, state)
      case o: OpPop => execute(o, state)
      case o: OpNargs => execute(o, state)
      /*
      case o: OpAlloc => execute(o, state)
      case o: OpPushAlloc => execute(o, state)
      case o: OpExtend => execute(o, state)
      case o: OpOutstanding => execute(o, state)
      case o: OpFork => execute(o, state)
      case o: OpXmitTag => execute(o, state)
      case o: OpXmitArg => execute(o, state)
      case o: OpXmitReg => execute(o, state)
      case o: OpXmit => execute(o, state)
      case o: OpXmitTagXtnd => execute(o, state)
      case o: OpXmitArgXtnd => execute(o, state)
      case o: OpXmitRegXtnd => execute(o, state)
      case o: OpSend => execute(o, state)
      case o: OpApplyPrimTag => execute(o, state)
      case o: OpApplyPrimArg => execute(o, state)
      case o: OpApplyPrimReg => execute(o, state)
      case o: OpApplyCmd => execute(o, state)
      case o: OpRtnTag => execute(o, state)
      case o: OpRtnArg => execute(o, state)
      case o: OpRtnReg => execute(o, state)
      case o: OpRtn => execute(o, state)
      case o: OpUpcallRtn => execute(o, state)
      case o: OpResume => execute(o, state)
      case o: OpNxt => execute(o, state)
      case o: OpJmp => execute(o, state)
      case o: OpJmpFalse => execute(o, state)
      case o: OpJmpCut => execute(o, state)
      case o: OpLookupToArg => execute(o, state)
      case o: OpLookupToReg => execute(o, state)
      case o: OpXferLexToArg => execute(o, state)
      case o: OpXferLexToReg => execute(o, state)
      case o: OpXferGlobalToArg => execute(o, state)
      case o: OpXferGlobalToReg => execute(o, state)
      case o: OpXferArgToArg => execute(o, state)
      case o: OpXferRsltToArg => execute(o, state)
      case o: OpXferArgToRslt => execute(o, state)
      case o: OpXferRsltToReg => execute(o, state)
      case o: OpXferRegToRslt => execute(o, state)
      case o: OpXferRsltToDest => execute(o, state)
      case o: OpXferSrcToRslt => execute(o, state)
      case o: OpIndLitToArg => execute(o, state)
      case o: OpIndLitToReg => execute(o, state)
      case o: OpIndLitToRslt => execute(o, state)
      case o: OpImmediateLitToArg => execute(o, state)
      case o: OpImmediateLitToReg => execute(o, state)
      case o: OpUnknown => execute(o, state)
     */
    }

  def execute(op: OpHalt, state: VMState): VMState =
    state.set(_ >> 'exitFlag)(true)

  def execute(op: OpPush, state: VMState): VMState =
    // TODO: Fix state.ctxt.get
    state.set(_ >> 'ctxt)(Ctxt.create(None, state.ctxt.get))

  def execute(op: OpPop, state: VMState): VMState =
    state.set(_ >> 'ctxt)(state.ctxt.get.ctxt)

  def execute(op: OpNargs, state: VMState): VMState =
    // TODO: Fix state.ctxt.get
    state.set(_ >> 'ctxt)(Some(state.ctxt.get.copy(nargs = op.n)))

  /*
  def execute(op : OpNargs, state : VMState) = {
    state.ctxt.get.argvec = Tuple.create(op.n, None)
  }

  def execute(op : OpPushAlloc, state : VMState) = {
    state.ctxt = Ctxt.create(Tuple.create(op.n, None), state.ctxt)
  }

  def execute(op : OpExtend, state : VMState) = {
    // stuff w/ op.v
  }

  def execute(op : OpOutstanding, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.pc = PC.fromInt(op.p)
    ctxt.outstanding = op.n
  }

  def execute(op : OpFork, state : VMState) = {
    var newCtxt = state.ctxt.get.clone()
    newCtxt.pc = PC.fromInt(op.p)
    state.strandPool.push(newCtxt)
  }

  def execute(op : OpXmitTag, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    ctxt.tag.atom = state.code.get.lit(op.v)
    state.xmitData = (op.u, op.n)
    state.doXmitFlag = true
  }

  def execute(op : OpXmitArg, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    ctxt.tag = ArgReg(op.a)
    state.xmitData = (op.u, op.n)
    state.doXmitFlag = true
  }

  def execute(op : OpXmitReg, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    ctxt.tag = CtxtReg(op.r)
    state.xmitData = (op.u, op.n)
    state.doXmitFlag = true
  }

  def execute(op : OpXmit, state : VMState) = {
    state.ctxt.get.nargs = op.m
    state.xmitData = (op.u, op.n)
    state.doXmitFlag = true
  }

  def execute(op : OpXmitTagXtnd, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    ctxt.tag.atom = state.code.get.lit(op.v)
    state.xmitData = (op.u, op.n)
    state.doXmitFlag = true
  }

  def execute(op : OpXmitArgXtnd, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    ctxt.tag = ArgReg(op.a)
    state.xmitData = (op.u, op.n)
    state.doXmitFlag = true
  }

  def execute(op : OpXmitReg, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    ctxt.tag = CtxtReg(op.r)
    state.xmitData = (op.u, op.n)
    state.doXmitFlag = true
  }

  def execute(op : OpSend, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.ctxt = None
    ctxt.nargs = op.m
    state.xmitData = (op.u, op.n)
    state.doXmitFlag = true
  }

  def execute(op : OpApplyPrimTag, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    val prim = Prim.nthPrim(op.k)
    state.loc.atom = state.code.get.lit(op.v)
    val result = if (op.u) { unwindAndApply(prim) }
    else { prim.get.dispatchHelper(state.ctxt) }
    if (result == DEADTHREAD) {
      state.doNextThreadFlag = true
    } else if (result.is(OTsysval)) {
      handleException(result, op, state.loc)
      state.doNextThreadFlag = true
    } else if (store(state.loc, state.ctxt, result)) {
      state.vmErrorFlag = true
    } else if (op.n) {
      state.doNextThreadFlag = true
    }
  }

  def execute(op : OpApplyPrimArg, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    val prim = Prim.nthPrim(op.k)
    val argno = op.a
    val result = if (op.u) { unwindAndApplyPrim(prim) }
    else { prim.get.dispatchHelper(state.ctxt) }
    if (result == DEADTHREAD) {
      state.doNextThreadFlag = true
    } else if (result.is(OTsysval)) {
      handleException(result, op, state.loc)
      state.doNextThreadFlag = true
    } else if (argno >= ARG_LIMIT) {
      state.vmErrorFlag = true
    } else {
      ctxt.argvec.get.elem(argno) = result
      if (op.n) {
        state.doNextThreadFlag = true
      }
    }
  }

  def execute(op : OpApplyPrimReg, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    val prim = Prim.nthPrim(op.k)
    val regno = op.r
    val result = if (op.u) { unwindAndApplyPrim(prim) }
    else { prim.get.dispatchHelper(state.ctxt) }
    if (result == DEADTHREAD) {
      state.doNextThreadFlag = true
    } else if (result.is(OTsysval)) {
      handleException(result, op, Location.CtxtReg(regno))
      state.doNextThreadFlag = true
    } else if (argno >= ARG_LIMIT) {
      state.vmErrorFlag = true
    } else {
      ctxt.reg(regno) = result
      if (op.n) {
        state.doNextThreadFlag = true
      }
    }
  }

  def execute(op : OpApplyCmd, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.nargs = op.m
    val prim = Prim.nthPrim(op.k)
    val result = if (op.u) { unwindAndApplyPrim(prim) }
    else { prim.get.dispatchHelper(state.ctxt) }
    if (result == DEADTHREAD) {
      state.doNextThreadFlag = true
    } else if (result.is(OTsysval)) {
      handleException(result, op, Location.CtxtReg(regno))
      state.doNextThreadFlag = true
    } else if (op.n) {
      state.doNextThreadFlag = true
    }
  }

  def execute(op : OpRtn, state : VMState) = {
    state.doRtnData = op.n
    state.doRtnFlag = true
  }

  def execute(op : OpRtnTag, state : VMState) = {
    state.ctxt.get.tag.atom = op.v
    state.doRtnData = op.n
    state.doRtnFlag = true
  }

  def execute(op : OpRtnArg, state : VMState) = {
    state.ctxt.get.tag = ArgReg(op.a)
    state.doRtnData = op.n
    state.doRtnFlag = true
  }

  def execute(op : OpRtnReg, state : VMState) = {
    state.ctxt.get.tag = CtxtReg(op.r)
    state.doRtnData = op.n
    state.doRtnFlag = true
  }

  def execute(op : OpRtnReg, state : VMState) = {
    val optCtxt = state.ctxt.get
    optCtxt.tag.atom = state.code.get.lit(op.v)
    if (optCtxt.tag.store(optCtxt.ctxt, optCtxt.rslt)) {
      state.vmErrorFlag = true
    } else if (n) {
      state.doNextThreadFlag = true
    }
  }

  def execute(op : OpRtnReg, state : VMState) = {
    state.ctxt.get.ctxt.get.scheduleStrand()
  }

  def execute(op : OpJmpCut, state : VMState) = {
    state.doNextThreadFlag = true
  }

  def execute(op : OpJmp, state : VMState) = {
    state.pc.absolute = code.get.absolutize(op.n)
  }

  def execute(op : OpJmpCut, state : VMState) = {
    var cut = op.m
    var newEnv : Option[Env] = state.ctxt.get.parent()
    while(0 < cut) {
      newEnv = newEnv.get.parent()
      cut -= 1
    }
    state.ctxt.get.env = newEnv
    state.pc.absolute = state.code.get.absolutize(op.n)
  }

  def execute(op : OpJmpFalse, state : VMState) = {
    if (state.ctxt.get.rslt == false) {
      state.pc.absolute = state.code.get.absolutize(op.n)
    }
  }

  def execute(op : OpLookupToArg, state : VMState) = {
    val argno = op.a
    val key : Option[Ob] = Some(state.code.get.lit(op.v))
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
    state.ctxt.get.argvec.get.elem(op.a) =
      state.GlobalEnv.get.entry(op.g)
  }

  execute(op : OpXferGlobalToReg, state : VMState) = {
    state.ctxt.get.reg(op.r) = state.GlobalEnv.get.entry(op.g)
  }

  execute(op : OpXferArgToArg, state : VMState) = {
    val argvec = state.ctxt.get.argvec.get
    argvec.elem(op.d) = argvec.elem(op.s)
  }

  execute(op : OpXferRsltToArg, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.argvec.get.elem(op.a) = ctxt.rslt
  }

  execute(op : OpXferArgToRslt, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.rslt = ctxt.argvec.get.elem(op.a)
  }

  execute(op : OpXferRsltToReg, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.reg(op.r) = ctxt.rslt
  }

  execute(op : OpXferRegToRslt, state : VMState) = {
    val ctxt = state.ctxt.get
    ctxt.rslt = ctxt.reg(op.r)
  }

  execute(op : OpXferRsltToDest, state : VMState) = {
    state.loc.atom = state.code.get.lit(op.v)
    if (store(state.loc, state.ctxt, state.ctxt.get.rslt)) {
      state.vmErrorFlag = true
    }
  }

  execute(op : OpXferSrcToRslt, state : VMState) = {
    state.loc.atom = state.code.get.lit(op.v)
    state.ctxt.get.rslt = fetch(state.loc, state.ctxt)
  }

  execute(op : OpIndLitToArg, state : VMState) = {
    state.ctxt.get.argvec.get.elem(op.a) = state.code.get.lit(op.v)
  }

  execute(op : OpIndLitToReg, state : VMState) = {
    state.ctxt.get.reg(op.r) = state.code.get.lit(op.v)
  }

  execute(op : OpIndLitToRslt, state : VMState) = {
    state.ctxt.get.rslt = state.code.get.lit(op.v)
  }

  execute(op : OpImmediateLitToArg, state : VMState) = {
    state.ctxt.get.argvec.get.elem(op.a) = vmLiterals[op.v]
  }

  execute(op : OpImmediateLitToReg, state : VMState) = {
    state.ctxt.get.reg(op.r) = vmLiterals[op.v]
  }

  execute(op : OpUnknown, state : VMState) = {
    //
    doNextThreadFlag = true
  }
 */
}
