package coop.rchain.rosette

sealed trait Work
case object NoWorkLeft extends Work
case object WaitForAsync extends Work
case class StrandsScheduled(state: VMState) extends Work

trait VirtualMachine {

  def unwindAndApplyPrim(prim: Prim): Ob = Ob.PLACEHOLDER
  def handleException(result: Ob, op: Op, loc: Location): Ob = Ob.PLACEHOLDER
  def handleFormalsMismatch(formals: Template): Ob = Ob.PLACEHOLDER
  def handleMissingBinding(key: Ob, argReg: Location): Ob = Ob.PLACEHOLDER
  val vmLiterals: Seq[Ob] = new Array[Ob](0)

  def getNextStrand(state: VMState): (Boolean, VMState) =
    if (state.strandPool.isEmpty) {
      tryAwakeSleepingStrand(state) match {
        case WaitForAsync =>
          val newState = state.set(_ >> 'doAsyncWaitFlag)(true)
          (false, newState)

        case NoWorkLeft => (true, state)

        case StrandsScheduled(stateScheduled) =>
          if (state.debug) {
            state.update(_ >> 'debugInfo)(_ :+ "*** waking sleepers\n")
          }

          val strand = stateScheduled.strandPool.head
          val newState = stateScheduled.update(_ >> 'strandPool)(_.tail)

          (false, installStrand(strand, newState))
      }
    } else {
      val strand = state.strandPool.head
      val newState = state.update(_ >> 'strandPool)(_.tail)

      (false, installStrand(strand, newState))
    }

  def tryAwakeSleepingStrand(state: VMState): Work =
    if (state.sleeperPool.isEmpty) {
      if (state.nsigs == 0) {
        NoWorkLeft
      } else {
        WaitForAsync
      }
    } else {

      /** Schedule all sleeping strands
        *
        * Pop strand from sleeperPool and enqueue
        * to strandPool
        */
      val scheduled = state.sleeperPool
        .foldLeft(state) {
          case (st, sleeper) =>
            sleeper.scheduleStrand(st)
        }
        .set(_ >> 'sleeperPool)(Seq())

      StrandsScheduled(scheduled)
    }

  def installStrand(strand: Ctxt, state: VMState): VMState = {
    val stateInstallMonitor =
      if (strand.monitor != state.currentMonitor)
        installMonitor(strand.monitor, state)
      else state

    installCtxt(strand, stateInstallMonitor)
  }

  def installMonitor(monitor: Monitor, state: VMState): VMState = {
    if (state.debug) {
      state.update(_ >> 'debugInfo)(_ :+ s"*** new monitor: ${monitor.id}\n")
    }

    state.currentMonitor.stop()

    val newState = state
      .set(_ >> 'bytecodes)(monitor.opcodeCounts)
      .set(_ >> 'currentMonitor)(monitor)
      .set(_ >> 'debug)(monitor.tracing)
      .set(_ >> 'obCounts)(monitor.obCounts)

    newState.currentMonitor
      .start()

    newState
  }

  def installCtxt(ctxt: Ctxt, state: VMState): VMState = {
    if (state.debug) {
      state.update(_ >> 'debugInfo)(_ :+ "*** new strand\n")
    }

    state
      .set(_ >> 'ctxt)(ctxt)
      .set(_ >> 'code)(ctxt.code)
      .set(_ >> 'pc >> 'relative)(ctxt.pc.relative)
  }

  def executeSeq(opCodes: Seq[Op], state: VMState): VMState = {
    var pc = 0
    var exit = false
    var currentState = state

    while (pc < opCodes.size && !exit) {
      val op = opCodes(pc)

      currentState = modifyFlags(executeDispatch(op, state))
        .update(_ >> 'bytecodes)(m =>
          m.updated(op, currentState.bytecodes.getOrElse(op, 0.toLong) + 1))

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
      //if (mState.ctxt.ret(mState.ctxt.rslt)) {
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
      case o: OpPushAlloc => execute(o, state)
      case o: OpExtend => execute(o, state)
      case o: OpOutstanding => execute(o, state)
      case o: OpAlloc => execute(o, state)
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
      case o: OpUpcallResume => execute(o, state)
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
    }

  def execute(op: OpHalt, state: VMState): VMState =
    state.set(_ >> 'exitFlag)(true)

  def execute(op: OpPush, state: VMState): VMState =
    state.set(_ >> 'ctxt)(Ctxt.create(Tuple.NIL, state.ctxt))

  def execute(op: OpPop, state: VMState): VMState =
    state.set(_ >> 'ctxt)(state.ctxt.ctxt)

  def execute(op: OpNargs, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'nargs)(op.n)

  def execute(op: OpAlloc, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'argvec)(Tuple.create(op.n, Ctxt.NIV))

  def execute(op: OpPushAlloc, state: VMState): VMState = {
    val ctxt = Ctxt.create(Tuple.create(op.n, Ob.NIV), state.ctxt)
    state.set(_ >> 'ctxt)(ctxt)
  }

  def execute(op: OpExtend, state: VMState): VMState = {
    val formals = state.code.lit(op.v).asInstanceOf[Template]
    val actuals = formals.matchPattern(state.ctxt.argvec, state.ctxt.nargs)

    actuals match {
      case Some(tuple) =>
        state
          .set(_ >> 'ctxt >> 'nargs)(0)
          .set(_ >> 'ctxt >> 'env)(
            state.ctxt.env.extendWith(formals.keymeta, tuple))

      case None =>
        handleFormalsMismatch(formals)
        state.set(_ >> 'doNextThreadFlag)(true)
    }
  }

  def execute(op: OpOutstanding, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'pc)(PC.fromInt(op.p))
      .set(_ >> 'ctxt >> 'outstanding)(op.n)

  def execute(op: OpFork, state: VMState): VMState =
    state.set(_ >> 'strandPool)(
      state.ctxt.copy(pc = PC.fromInt(op.p)) +: state.strandPool)

  def execute(op: OpXmitTag, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag >> 'atom)(state.code.lit(op.v))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitArg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.a))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitReg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.r))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmit, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitTagXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag >> 'atom)(state.code.lit(op.v))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitArgXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.a))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpXmitRegXtnd, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.r))
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpSend, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'ctxt)(Ctxt.NIV)
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'xmitData)((op.u, op.n))
      .set(_ >> 'doXmitFlag)(true)

  def execute(op: OpApplyPrimTag, state: VMState): VMState = {
    val newState = state
      .set(_ >> 'ctxt >> 'nargs)(op.m)
      .set(_ >> 'loc >> 'atom)(state.code.lit(op.v))
    val prim = Prim.nthPrim(op.k)
    val result = if (op.u) { unwindAndApplyPrim(prim) } else {
      prim.dispatchHelper(state.ctxt)
    }
    if (result == Ob.DEADTHREAD) {
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else if (result.is(Ob.OTsysval)) {
      handleException(result, op, newState.loc)
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else if (Location.store(state.loc, state.ctxt, result)) {
      newState.set(_ >> 'vmErrorFlag)(true)
    } else if (op.n) {
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else {
      newState
    }
  }

  def execute(op: OpApplyPrimArg, state: VMState): VMState = {
    val newState = state.set(_ >> 'ctxt >> 'nargs)(op.m)
    val prim = Prim.nthPrim(op.k)
    val argno = op.a
    val result = if (op.u) { unwindAndApplyPrim(prim) } else {
      prim.dispatchHelper(state.ctxt)
    }
    if (result == Ob.DEADTHREAD) {
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else if (result.is(Ob.OTsysval)) {
      handleException(result, op, newState.loc)
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else if (argno >= state.ctxt.argvec.elem.length) {
      newState.set(_ >> 'vmErrorFlag)(true)
    } else {
      val thirdState =
        newState.update(_ >> 'ctxt >> 'argvec >> 'elem)(
          _.updated(argno, result))
      if (op.n) {
        thirdState.set(_ >> 'doNextThreadFlag)(true)
      } else {
        thirdState
      }
    }
  }

  def execute(op: OpApplyPrimReg, state: VMState): VMState = {
    val newState = state.set(_ >> 'ctxt >> 'nargs)(op.m)
    val prim = Prim.nthPrim(op.k)
    val regno = op.r
    val result = if (op.u) { unwindAndApplyPrim(prim) } else {
      prim.dispatchHelper(state.ctxt)
    }
    if (result == Ob.DEADTHREAD) {
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else if (result.is(Ob.OTsysval)) {
      handleException(result, op, Location.CtxtReg(regno))
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else {
      val thirdState =
        newState.update(_ >> 'ctxt >> 'reg)(_.updated(regno, result))
      if (op.n) {
        thirdState.set(_ >> 'doNextThreadFlag)(true)
      } else {
        thirdState
      }
    }
  }

  def execute(op: OpApplyCmd, state: VMState): VMState = {
    val newState = state.set(_ >> 'ctxt >> 'nargs)(op.m)
    val prim = Prim.nthPrim(op.k)
    val result = if (op.u) { unwindAndApplyPrim(prim) } else {
      prim.dispatchHelper(state.ctxt)
    }
    if (result == Ob.DEADTHREAD) {
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else if (result.is(Ob.OTsysval)) {
      handleException(result, op, Location.LIMBO)
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else if (op.n) {
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else {
      newState
    }
  }

  def execute(op: OpRtn, state: VMState): VMState =
    state
      .set(_ >> 'doRtnData)(op.n)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnTag, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag >> 'atom)(state.code.lit(op.v))
      .set(_ >> 'doRtnData)(op.n)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnArg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(Location.ArgReg(op.a))
      .set(_ >> 'doRtnData)(op.n)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpRtnReg, state: VMState): VMState =
    state
      .set(_ >> 'ctxt >> 'tag)(Location.CtxtReg(op.r))
      .set(_ >> 'doRtnData)(op.n)
      .set(_ >> 'doRtnFlag)(true)

  def execute(op: OpUpcallRtn, state: VMState): VMState = {
    val newState = state.set(_ >> 'ctxt >> 'tag >> 'atom)(state.code.lit(op.v))
    val ctxt = newState.ctxt
    if (Location.store(ctxt.tag, ctxt.ctxt, ctxt.rslt)) {
      newState.set(_ >> 'vmErrorFlag)(true)
    } else if (op.n) {
      newState.set(_ >> 'doNextThreadFlag)(true)
    } else {
      newState
    }
  }

  def execute(op: OpUpcallResume, state: VMState): VMState =
    state.ctxt.ctxt
      .scheduleStrand(state)
      .set(_ >> 'doNextThreadFlag)(true)

  def execute(op: OpNxt, state: VMState): VMState = {
    val (exit, newState) = getNextStrand(state)

    if (exit) {
      newState.set(_ >> 'exitFlag)(true)
    } else {
      newState
    }
  }

  def execute(op: OpJmp, state: VMState): VMState =
    state.set(_ >> 'pc >> 'relative)(op.n)

  def execute(op: OpJmpCut, state: VMState): VMState = {
    val cut = op.m

    val env = (1 to cut).foldLeft(state.ctxt.env)((env, _) => env.parent())

    state
      .set(_ >> 'ctxt >> 'env)(env)
      .set(_ >> 'pc >> 'relative)(op.n)
  }

  def execute(op: OpJmpFalse, state: VMState): VMState =
    if (state.ctxt.rslt == Ob.FALSE) {
      state.set(_ >> 'pc >> 'relative)(op.n)
    } else {
      state
    }

  def execute(op: OpLookupToArg, state: VMState): VMState = {
    val argno = op.a
    val key = state.code.lit(op.v)

    val value =
      state.ctxt.selfEnv.meta.lookupOBO(state.ctxt.selfEnv, key, state.ctxt)

    value match {
      case Left(Upcall) =>
        state
          .set(_ >> 'doNextThreadFlag)(true)
      case Left(Absent) =>
        handleMissingBinding(key, Location.ArgReg(argno))
        state
          .set(_ >> 'doNextThreadFlag)(true)

      case Right(ob) =>
        state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(argno, ob))
    }
  }

  def execute(op: OpLookupToReg, state: VMState): VMState = {
    val regno = op.r
    val key = state.code.lit(op.v)

    val value =
      state.ctxt.selfEnv.meta.lookupOBO(state.ctxt.selfEnv, key, state.ctxt)

    value match {
      case Left(Upcall) =>
        state
          .set(_ >> 'doNextThreadFlag)(true)
      case Left(Absent) =>
        handleMissingBinding(key, Location.CtxtReg(regno))
        state
          .set(_ >> 'doNextThreadFlag)(true)

      case Right(ob) =>
        state.update(_ >> 'ctxt >> 'reg)(_.updated(regno, ob))
    }
  }

  def execute(op: OpXferLexToArg, state: VMState): VMState = {
    val level = op.l

    val env = (1 to level).foldLeft(state.ctxt.env)((env, _) => env.parent())

    val slot = if (op.i) {
      val actor = Actor(env)
      actor.extension.slot
    } else {
      env.slot
    }

    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(_.updated(op.a, slot(op.o)))
  }

  def execute(op: OpXferLexToReg, state: VMState): VMState = {
    val level = op.l

    val env = (1 to level).foldLeft(state.ctxt.env)((env, _) => env.parent())

    val slot = if (op.i) {
      val actor = Actor(env)
      actor.extension.slot
    } else {
      env.slot
    }

    state.update(_ >> 'ctxt >> 'reg)(_.updated(op.r, slot(op.o)))
  }

  def execute(op: OpXferGlobalToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, state.GlobalEnv.entry(op.g)))

  def execute(op: OpXferGlobalToReg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'reg)(
      _.updated(op.r, state.GlobalEnv.entry(op.g)))

  def execute(op: OpXferArgToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.d, state.ctxt.argvec.elem(op.s)))

  def execute(op: OpXferRsltToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, state.ctxt.rslt))

  def execute(op: OpXferArgToRslt, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'rslt)(state.ctxt.argvec.elem(op.a))

  def execute(op: OpXferRsltToReg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'reg)(_.updated(op.r, state.ctxt.rslt))

  def execute(op: OpXferRegToRslt, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'rslt)(state.ctxt.reg(op.r))

  def execute(op: OpXferRsltToDest, state: VMState): VMState = {
    val newState = state.set(_ >> 'loc >> 'atom)(state.code.lit(op.v))
    if (Location.store(newState.loc, newState.ctxt, newState.ctxt.rslt)) {
      newState.set(_ >> 'vmErrorFlag)(true)
    } else {
      newState
    }
  }

  def execute(op: OpXferSrcToRslt, state: VMState): VMState =
    state
      .set(_ >> 'loc >> 'atom)(state.code.lit(op.v))
      .set(_ >> 'ctxt >> 'rslt)(Location.fetch(state.loc, state.ctxt))

  def execute(op: OpIndLitToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, state.code.lit(op.v)))

  def execute(op: OpIndLitToReg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'reg)(_.updated(op.r, state.code.lit(op.v)))

  def execute(op: OpIndLitToRslt, state: VMState): VMState =
    state.set(_ >> 'ctxt >> 'rslt)(state.code.lit(op.v))

  def execute(op: OpImmediateLitToArg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'argvec >> 'elem)(
      _.updated(op.a, vmLiterals(op.v)))

  def execute(op: OpImmediateLitToReg, state: VMState): VMState =
    state.update(_ >> 'ctxt >> 'reg)(_.updated(op.r, vmLiterals(op.v)))

  def execute(op: OpUnknown, state: VMState): VMState =
    state.set(_ >> 'doNextThreadFlag)(true)
}
