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
      case o: OpHalt      => execute(o, state)
      case o: OpPush      => execute(o, state)
      case o: OpPop       => execute(o, state)
      case o: OpNargs     => execute(o, state)
      case o: OpPushAlloc => execute(o, state)
      //case o: OpExtend => execute(o, state)
      case o: OpOutstanding  => execute(o, state)
      case o: OpAlloc        => execute(o, state)
      case o: OpFork         => execute(o, state)
      case o: OpXmitTag      => execute(o, state)
      case o: OpXmitArg      => execute(o, state)
      case o: OpXmitReg      => execute(o, state)
      case o: OpXmit         => execute(o, state)
      case o: OpXmitTagXtnd  => execute(o, state)
      case o: OpXmitArgXtnd  => execute(o, state)
      case o: OpXmitRegXtnd  => execute(o, state)
      case o: OpSend         => execute(o, state)
      case o: OpApplyPrimTag => execute(o, state)
      case o: OpApplyPrimArg => execute(o, state)
      case o: OpApplyPrimReg => execute(o, state)
      case o: OpApplyCmd     => execute(o, state)
      case o: OpRtnTag       => execute(o, state)
      case o: OpRtnArg       => execute(o, state)
      case o: OpRtnReg       => execute(o, state)
      case o: OpRtn          => execute(o, state)
      case o: OpUpcallRtn    => execute(o, state)
      case o: OpResume       => execute(o, state)
      case o: OpNxt          => execute(o, state)
      case o: OpJmp          => execute(o, state)
      case o: OpJmpFalse     => execute(o, state)
      /*
      case o: OpJmpCut => execute(o, state)
      case o: OpLookupToArg => execute(o, state)
      case o: OpLookupToReg => execute(o, state)
      case o: OpXferLexToArg => execute(o, state)
      case o: OpXferLexToReg => execute(o, state)
       */
      case o: OpXferGlobalToArg   => execute(o, state)
      case o: OpXferGlobalToReg   => execute(o, state)
      case o: OpXferArgToArg      => execute(o, state)
      case o: OpXferRsltToArg     => execute(o, state)
      case o: OpXferArgToRslt     => execute(o, state)
      case o: OpXferRsltToReg     => execute(o, state)
      case o: OpXferRegToRslt     => execute(o, state)
      case o: OpXferRsltToDest    => execute(o, state)
      case o: OpXferSrcToRslt     => execute(o, state)
      case o: OpIndLitToArg       => execute(o, state)
      case o: OpIndLitToReg       => execute(o, state)
      case o: OpIndLitToRslt      => execute(o, state)
      case o: OpImmediateLitToArg => execute(o, state)
      case o: OpImmediateLitToReg => execute(o, state)
      case o: OpUnknown           => execute(o, state)
    }

  def execute(op: OpHalt, state: VMState): VMState =
    state.set(_ >> 'exitFlag)(true)

  def execute(op: OpPush, state: VMState): VMState =
    // TODO: Fix state.ctxt.get
    state.set(_ >> 'ctxt)(Ctxt.create(None, state.ctxt))

  def execute(op: OpPop, state: VMState): VMState =
    state.set(_ >> 'ctxt)(state.ctxt.ctxt)

  def execute(op: OpNargs, state: VMState): VMState =
    // TODO: Fix state.ctxt.get
    state.set(_ >> 'ctxt)(Some(state.ctxt.copy(nargs = op.n)))

  def execute(op: OpPushAlloc, state: VMState): VMState = {
    // TODO: Fix state.ctxt.get
    val ctxt = Ctxt.create(Some(Tuple.create(op.n, None)), state.ctxt.get)
    state.set(_ >> 'ctxt)(ctxt)
  }

  /*
  def execute(op : OpExtend, state : VMState) = {
    // stuff w/ op.v
  }
   */

  def execute(op: OpOutstanding, state: VMState): VMState = {
    state
      .set(_ << 'ctxt << 'pc)(PC.fromInt(op.p))
      .set(_ << 'ctxt << 'outstanding)(op.n)
  }

  def execute(op: OpFork, state: VMState) = {
    var newCtxt = state.ctxt.clone();
    newCtxt.pc = PC.fromInt(op.p)
    // Assumes that push returns a new pool instead of modifying in place
    state.set(_ << 'strandPool)(state.strandPool.push(newCtxt))
  }

  def execute(op: OpXmitTag, state: VMState) = {
    state
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'ctxt << 'tag << 'atom)(state.code.lit(op.v))
      .set(_ << 'xmitData)((op.u, op.n))
      .set(_ << 'doXmitFlag)(true)
  }

  def execute(op: OpXmitArg, state: VMState) = {
    state
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'ctxt << 'tag)(ArgReg(op.a))
      .set(_ << 'xmitData)((op.u, op.n))
      .set(_ << 'doXmitFlag)(true)
  }

  def execute(op: OpXmitReg, state: VMState) = {
    state
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'ctxt << 'tag)(CtxtReg(op.r))
      .set(_ << 'xmitData)((op.u, op.n))
      .set(_ << 'doXmitFlag)(true)
  }

  def execute(op: OpXmit, state: VMState) = {
    state
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'xmitData)((op.u, op.n))
      .set(_ << 'doXmitFlag)(true)
  }

  def execute(op: OpXmitTagXtnd, state: VMState) = {
    state
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'ctxt << 'tag << 'atom)(state.code.lit(op.v))
      .set(_ << 'xmitData)((op.u, op.n))
      .set(_ << 'doXmitFlag)(true)
  }

  def execute(op: OpXmitArgXtnd, state: VMState) = {
    state
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'ctxt << 'tag)(ArgReg(op.a))
      .set(_ << 'xmitData)((op.u, op.n))
      .set(_ << 'doXmitFlag)(true)
  }

  def execute(op: OpXmitRegXtnd, state: VMState) = {
    state
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'ctxt << 'tag)(CtxtReg(op.r))
      .set(_ << 'xmitData)((op.u, op.n))
      .set(_ << 'doXmitFlag)(true)
  }

  def execute(op: OpSend, state: VMState) = {
    state
      .set(_ << 'ctxt << 'ctxt)(Ctxt.NIV)
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'xmitData)((op.u, op.n))
      .set(_ << 'doXmitFlag)(true)
  }

  def execute(op: OpApplyPrimTag, state: VMState) = {
    val newState = state
      .set(_ << 'ctxt << 'nargs)(op.m)
      .set(_ << 'loc << 'atom)(state.code.get.lit(op.v))
    val prim = Prim.nthPrim(op.k)
    val result = if (op.u) { unwindAndApply(prim) } else {
      prim.get.dispatchHelper(state.ctxt)
    }
    if (result == DEADTHREAD) {
      newState.set(_ << 'doNextThreadFlag)(true)
    } else if (result.is(OTsysval)) {
      handleException(result, op, newState.loc)
      newState.set(_ << 'doNextThreadFlag)(true)
    } else if (store(state.loc, state.ctxt, result)) {
      newState.set(_ << 'vmErrorFlag)(true)
    } else if (op.n) {
      newState.set(_ << 'doNextThreadFlag)(true)
    } else {
      newState
    }
  }

  def execute(op: OpApplyPrimArg, state: VMState) = {
    val newState = state.set(_ << 'ctxt << 'nargs)(op.m)
    val prim = Prim.nthPrim(op.k)
    val argno = op.a
    val result = if (op.u) { unwindAndApplyPrim(prim) } else {
      prim.get.dispatchHelper(state.ctxt)
    }
    if (result == DEADTHREAD) {
      newState.set(_ << 'doNextThreadFlag)(true)
    } else if (result.is(OTsysval)) {
      handleException(result, op, newState.loc)
      newState.set(_ << 'doNextThreadFlag)(true)
    } else if (argno >= ARG_LIMIT) {
      newState.set(_ << 'vmErrorFlag)(true)
    } else {
      val thirdState =
        newState.set(_ << 'ctxt << 'argvec << 'elem)(_.updated(argno, result))
      if (op.n) {
        thirdState.set(_ << 'doNextThreadFlag)(true)
      } else {
        thirdState
      }
    }
  }

  def execute(op: OpApplyPrimReg, state: VMState) = {
    val newState = state.set(_ << 'ctxt << 'nargs)(op.m)
    val prim = Prim.nthPrim(op.k)
    val regno = op.r
    val result = if (op.u) { unwindAndApplyPrim(prim) } else {
      prim.get.dispatchHelper(state.ctxt)
    }
    if (result == DEADTHREAD) {
      newState.set(_ << 'doNextThreadFlag)(true)
    } else if (result.is(OTsysval)) {
      handleException(result, op, Location.CtxtReg(regno))
      newState.set(_ << 'doNextThreadFlag)(true)
    } else if (argno >= ARG_LIMIT) {
      newState.set(_ << 'vmErrorFlag)(true)
    } else {
      val thirdState =
        newState.set(_ << 'ctxt << 'reg)(_.updated(regno, result))
      if (op.n) {
        thirdState.set(_ << 'doNextThreadFlag)(true)
      } else {
        thirdState
      }
    }
  }

  def execute(op: OpApplyCmd, state: VMState) = {
    val newState = state.set(_ << 'ctxt << 'nargs)(op.m)
    val prim = Prim.nthPrim(op.k)
    val result = if (op.u) { unwindAndApplyPrim(prim) } else {
      prim.get.dispatchHelper(state.ctxt)
    }
    if (result == DEADTHREAD) {
      newState.set(_ << 'doNextThreadFlag)(true)
    } else if (result.is(OTsysval)) {
      handleException(result, op, Location.CtxtReg(regno))
      newState.set(_ << 'doNextThreadFlag)(true)
    } else if (op.n) {
      newState.set(_ << 'doNextThreadFlag)(true)
    } else {
      newState
    }
  }

  def execute(op: OpRtn, state: VMState) = {
    state.set(_ << 'doRtnData)(op.n).set(_ << 'doRtnFlag)(true)
  }

  def execute(op: OpRtnTag, state: VMState) = {
    state
      .set(_ << 'ctxt << 'tag << 'atom)(op.v)
      .set(_ << 'doRtnData)(op.n)
      .set(_ << 'doRtnFlag)(true)
  }

  def execute(op: OpRtnArg, state: VMState) = {
    state
      .set(_ << 'ctxt << 'tag)(ArgReg(op.a))
      .set(_ << 'doRtnData)(op.n)
      .set(_ << 'doRtnFlag)(true)
  }

  def execute(op: OpRtnReg, state: VMState) = {
    state
      .set(_ << 'ctxt << 'tag)(CtxtReg(op.r))
      .set(_ << 'doRtnData)(op.n)
      .set(_ << 'doRtnFlag)(true)
  }

  def execute(op: OpRtnReg, state: VMState) = {
    val newState = state.set(_ << 'ctxt << 'tag << 'atom)(state.code.lit(op.v))
    if (state.ctxt.tag.store(state.ctxt.ctxt, state.ctxt.rslt)) {
      newState.set(_ << 'vmErrorFlag)(true)
    } else if (op.n) {
      newState.set(_ << 'doNextThreadFlag)(true)
    } else {
      newState
    }
  }

  def execute(op: OpRtnReg, state: VMState) = {
    state.ctxt.ctxt.scheduleStrand()
  }

  def execute(op: OpJmpCut, state: VMState) = {
    state.set(_ << 'doNextThreadFlag)(true)
  }

  def execute(op: OpJmp, state: VMState) = {
    state.set(_ << 'pc << 'absolute)(code.absolutize(op.n))
  }

  def execute(op: OpJmpCut, state: VMState) = {
    var cut = op.m
    var newEnv = state.ctxt.parent()
    while (0 < cut) {
      newEnv = newEnv.parent()
      cut -= 1
    }
    state
      .set(_ << 'ctxt << 'env)(newEnv)
      .set(_ << 'pc << 'absolute)(newState.code.absolutize(op.n))
  }

  def execute(op: OpJmpFalse, state: VMState) = {
    if (state.ctxt.rslt == false) {
      state.set(_ << 'pc << 'absolute)(state.code.absolutize(op.n))
    } else {
      state
    }
  }

  /*
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
   */

  execute(op: OpXferGlobalToArg, state: VMState) = {
    state.set(_ << 'ctxt << 'argvec << 'elem)(
      _.updated(op.d, state.GlobalEnv.entry(op.g)))
  }

  execute(op: OpXferGlobalToReg, state: VMState) = {
    state.set(_ << 'ctxt << 'reg)(_.updated(op.r, state.GlobalEnv.entry(op.g)))
  }

  execute(op: OpXferArgToArg, state: VMState) = {
    state.set(_ << 'ctxt << 'argvec << 'elem)(
      _.updated(op.d, state.ctxt.argvec.elem(op.s)))
  }

  execute(op: OpXferRsltToArg, state: VMState) = {
    state.set(_ << 'ctxt << 'argvec << 'elem)(_.updated(op.a, state.ctxt.rslt))
  }

  execute(op: OpXferArgToRslt, state: VMState) = {
    state.set(_ << 'ctxt << 'rslt)(state.ctxt.argvec.elem(op.a))
  }

  execute(op: OpXferRsltToReg, state: VMState) = {
    state.set(_ << 'ctxt << 'reg)(_.updated(op.r, state.ctxt.rslt));
  }

  execute(op: OpXferRegToRslt, state: VMState) = {
    state.set(_ << 'ctxt << 'rslt)(state.ctxt.reg(op.r))
  }

  execute(op: OpXferRsltToDest, state: VMState) = {
    val newState = state.set(_ << 'loc << 'atom)(state.code.lit(op.v))
    if (store(newState.loc, newState.ctxt, newState.ctxt.rslt)) {
      newState.set(_ << 'vmErrorFlag)(true)
    } else {
      newState
    }
  }

  execute(op: OpXferSrcToRslt, state: VMState) = {
    state
      .set(_ << 'loc << 'atom)(state.code.lit(op.v))
      .set(_ << 'ctxt << 'rslt)(fetch(state.loc, state.ctxt))
  }

  execute(op: OpIndLitToArg, state: VMState) = {
    state.set(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, state.code.lit(op.v)))
  }

  execute(op: OpIndLitToReg, state: VMState) = {
    state.set(_ >> 'ctxt >> 'reg)(_.updated(op.r, state.code.lit(op.v)))
  }

  execute(op: OpIndLitToRslt, state: VMState) = {
    state.set(_ >> 'ctxt >> 'rslt)(state.code.lit(op.v))
  }

  execute(op: OpImmediateLitToArg, state: VMState) = {
    state.set(_ >> 'argvec >> 'elem)(_.updated(op.a, vmLiterals(op.v)))
  }

  execute(op: OpImmediateLitToReg, state: VMState) = {
    state.set(_ >> 'ctxt >> 'reg)(_.updated(op.r, vmLiterals(op.v)))
  }

  execute(op: OpUnknown, state: VMState) = {
    state.set(_ >> 'doNextThreadFlag)(true)
  }
}
