package coop.rchain.rosette

class VMState {
  var bytecodes : List[Int]
  var code : Option[Code]
  var ctxt : Option[Ctxt]
  var debuggingLevel : Int = 0
  var loc : Location
  var pc : PC
  var sigvec : Int = 0
  var strandPool : scala.collection.mutable.Stack[Ctxt]

  var nextOpFlag : Boolean = true
  var doXmitFlag : Boolean = false
  var xmitData : (Boolean, Boolean) = (false, false)
  var doRtnFlag : Boolean = false
  var doNextThread : Boolean = false
  var vmErrorFlag : Boolean = false
  var exitFlag : Boolean = false
}

