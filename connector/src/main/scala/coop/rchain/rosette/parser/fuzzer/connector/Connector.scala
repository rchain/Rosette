package coop.rchain.rosette.parser.fuzzer.connector

import java.io.{BufferedWriter, File, FileWriter}

import coop.rchain.rosette.parser.fuzzer.Fuzzer

object Connector {
  val filenameValid = "fuzzer-output-valid.txt"
  val filenameInvalid = "fuzzer-output-invalid.txt"

  def runTest(): Unit = {
    runTestValidGrammar()
    println()
    runTestInvalidGrammar()
  }

  def runTestValidGrammar(): Unit = {
    println("Generate valid test cases and test if parsers succeed in parsing")

    genTestCases(new File(filenameValid), 2000, valid = true)

    val result = Parser.haskell.runTest(filenameValid)

    result match {
      case Success(id) => println(s"$id: Success")
      case Failure(id, error) => println(s"$id: $error")
    }
  }

  def runTestInvalidGrammar(): Unit = {
    println("Generate invalid test cases and test if parsers fail")

    genTestCases(new File(filenameInvalid), 100, valid = false)

    val result = Parser.haskell.runTest(filenameInvalid)

    result match {
      case Success(id) => println(s"$id: unintentionally succeeded in parsing")
      case Failure(id, error) => println("Success")
    }
  }

  def genTestCases(file: File, nTerms: Int, valid: Boolean): Unit = {
    val bw = new BufferedWriter(new FileWriter(file))

    if (valid) {
      Fuzzer
        .fuzzyTermsValid()
        .take(nTerms)
        .foreach(line => bw.write(line + '\n'))
    } else {
      Fuzzer
        .fuzzyTermsInvalid()
        .take(nTerms)
        .foreach(line => bw.write(line + '\n'))
    }

    bw.close()
  }

  def main(args: Array[String]): Unit =
    runTest()
}
