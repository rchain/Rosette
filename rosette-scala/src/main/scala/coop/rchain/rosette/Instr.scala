package coop.rchain.rosette

class Instr extends Ob {
  var opcode : Op
  var args : List[Int] // Covers 16-bit args; may want 64-bit ones eventually
}

