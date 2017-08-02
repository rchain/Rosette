package coop.rchain.rosette.parser.fuzzer.connector

import sys.process._

sealed trait ParseResult
case class Success(parserId: String) extends ParseResult
case class Failure(parserId: String, error: String) extends ParseResult

trait Parser {
  val id: String

  val bnf: String

  val build: Seq[String]

  val run: String

  val buildDir: String

  lazy val dir = s"$buildDir/parserGen"

  def interpretOutput(output: String): ParseResult

  def runTest(inputFile: String): ParseResult = {
    s"mkdir -p $dir".!
    s"cp $bnf $dir".!

    build.map(
      cmd =>
        Process(cmd, new java.io.File(s"./$dir"))
          .run(ProcessLogger(_ => ()))
          .exitValue())

    val parseProcess = s"cat $inputFile" #| run

    val output =
      parseProcess.lineStream_!.foldLeft("")((s, line) => s ++ (line + '\n'))

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
          "no viable alternative at input")) Failure(id, "error")
    else Success(id)
}

trait Bnfc extends Parser {
  val buildDir = "target"

  val bnf = "connector/src/main/resources/Rbl.bnfc"

  val run = s"./$dir/TestRbl"

  override def interpretOutput(output: String): ParseResult =
    if (output.contains("Parse Successful!")) {
      Success(id)
    } else {
      val matchErrorRegex = "\n(syntax error at line.*)$".r
      val firstMatch = matchErrorRegex.findFirstMatchIn(output)

      firstMatch match {
        case Some(m) =>
          if (m.groupCount > 0) {
            Failure(id, m.group(1))
          } else {
            Failure(id, "Regex error")
          }
        case None => Failure(id, "Unknown error")
      }
    }
}

object Parser {
  val java = new Parser with Antlr4 {
    override val id: String = "Java Antlr4"

    override val build: Seq[String] = Seq(s"antlr4 Rbl.g4", "javac Rbl*.java")
  }

  val ocaml = new Parser with Bnfc {
    override val id: String = "Ocaml Bnfc"

    override val build: Seq[String] = Seq(s"bnfc -m -ocaml Rbl.bnfc", "make")
  }

  val haskell = new Parser with Bnfc {
    override val id: String = "Haskell Bnfc"

    override val build: Seq[String] = Seq(s"bnfc -m -haskell Rbl.bnfc", "make")
  }
}
