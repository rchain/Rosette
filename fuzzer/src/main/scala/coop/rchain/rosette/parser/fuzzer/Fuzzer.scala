package coop.rchain.rosette.parser.fuzzer

import java.io.{BufferedWriter, File, FileWriter}

import scala.util.Random

object Fuzzer {
  val filenameValid = "fuzzer-output-valid.txt"
  val filenameInvalid = "fuzzer-output-invalid.txt"

  def genTestCases(file: File,
                   nTerms: Int,
                   maxBreadth: Int,
                   maxDepth: Int,
                   valid: Boolean): Unit = {
    val bw = new BufferedWriter(new FileWriter(file))

    if (valid) {
      Fuzzer
        .fuzzyTermsValid(maxBreadth, maxDepth)
        .take(nTerms)
        .foreach(line => bw.write(line + '\n'))
    } else {
      Fuzzer
        .fuzzyTermsInvalid(maxBreadth, maxDepth)
        .take(nTerms)
        .foreach(line => bw.write(line + '\n'))
    }

    bw.close()
  }

  def fuzzyTermsValid(maxBreadth: Int = 5, maxDepth: Int = 5): Stream[String] =
    Stream.continually(
      FuzzyTerm.randomTerm(Bnf.valid, maxBreadth, maxDepth)(Random.nextLong))

  def fuzzyTermsInvalid(maxBreadth: Int = 5,
                        maxDepth: Int = 5): Stream[String] =
    Stream.continually(
      FuzzyTerm.randomTerm(Bnf.invalid, maxBreadth, maxDepth)(Random.nextLong))

  def main(args: Array[String]): Unit =
    if (args.length > 0) {

      val nTests = try {
        args(1).toInt
      } catch {
        case e: Exception => 2000
      }

      val maxBreadth = try {
        args(2).toInt
      } catch {
        case e: Exception => 5
      }

      val maxDepth = try {
        args(3).toInt
      } catch {
        case e: Exception => 5
      }

      args(0) match {
        case "valid" =>
          genTestCases(new File(filenameValid),
                       nTests,
                       maxBreadth,
                       maxDepth,
                       valid = true)
        case "invalid" =>
          genTestCases(new File(filenameInvalid),
                       nTests,
                       maxBreadth,
                       maxDepth,
                       valid = false)
      }
    } else {
      println("fuzzer/run valid nTests maxBreadth maxDepth")
      println("fuzzer/run invalid nTests maxBreadth maxDepth")
    }
}
