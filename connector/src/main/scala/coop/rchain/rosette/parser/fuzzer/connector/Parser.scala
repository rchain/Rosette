package coop.rchain.rosette.parser.fuzzer.connector

import sys.process._

sealed trait ParseResult
case object Success extends ParseResult
case object Failure extends ParseResult

trait Parser {
  val bnf: String

  val build: Seq[String]

  val run: String

  val buildDir: String

  lazy val dir = s"$buildDir/parserGen"

  def interpretOutput(string: String): ParseResult

  def runTest(inputFile: String): ParseResult = {
    s"mkdir -p $dir".!
    s"cp $bnf $dir".!

    build.map(cmd => Process(cmd, new java.io.File(s"./$dir")).!)

    val output = (s"cat $inputFile" #| run).!!

    s"rm -rf $dir".!!

    interpretOutput(output)
  }
}

trait Antlr4 extends Parser {
  val buildDir = "target"

  val bnf = "connector/src/main/resources/Rbl.g4"

  val run = s"grun $dir/Rbl program -tree"

  override def interpretOutput(output: String): ParseResult =
    if (output.contains("\nline") || output.contains(
          "no viable alternative at input")) Failure
    else Success
}

trait Bnfc extends Parser {
  val buildDir = "target"

  val bnf = "connector/src/main/resources/Rbl.bnfc"

  val run = s"./$dir/TestRbl"

  override def interpretOutput(output: String): ParseResult =
    if (output.contains("Parse Successful!")) Success else Failure

}

object Parser {
  val java = new Parser with Antlr4 {
    override val build: Seq[String] = Seq(s"antlr4 Rbl.g4", "javac Rbl*.java")
  }

  val ocaml = new Parser with Bnfc {
    override val build: Seq[String] = Seq(s"bnfc -m -ocaml Rbl.bnfc", "make")
  }

  val haskell = new Parser with Bnfc {
    override val build: Seq[String] = Seq(s"bnfc -m -haskell Rbl.bnfc", "make")
  }
}
