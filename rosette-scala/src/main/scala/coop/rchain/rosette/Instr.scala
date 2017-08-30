package coop.rchain.rosette

// Covers 16-bit args; may want 64-bit ones eventually
case class Instr(opcode: Op,
                 args: List[Int],
                 override val entry: Seq[Ob],
                 override val meta: Ob,
                 override val slot: Seq[Ob])
    extends Ob
