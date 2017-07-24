package coop.rchain.rosette.parser.fuzzer.connector

import coop.rchain.rosette.parser.fuzzer.Fuzzer

object Connector extends App {
  val testPositive = Fuzzer.getPositiveTestFile
  val testNegative = Fuzzer.getNegativeTestFile

  println(Parser.haskell.runTest("test-output.txt"))
}
