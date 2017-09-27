package coop.rchain.rosette

import java.io.InputStreamReader

import coop.rchain.rosette.parser.bytecode._
import Show._

object Main extends App {

  /*
  // TODO: Remove before merge
  val test = Seq[Int](
    Integer.parseInt("00000000", 2), // halt
    Integer.parseInt("00000001", 2), // ignored arg
    Integer.parseInt("00000011", 2), // nargs
    Integer.parseInt("00101010", 2), // nargs arg
    Integer.parseInt("00001011", 2), // outstanding
    Integer.parseInt("00000000", 2), // outstanding p arg
    Integer.parseInt("00000000", 2), // outstanding n arg
    Integer.parseInt("00001000", 2), // outstanding x arg
  )

  def writeFile(filename: String, bytes: Seq[Int]): Unit = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(bytes.map(_.toByte).toArray)
    bos.close()
  }

  writeFile("test.txt", test)
   */

  val bytes = collection.mutable.ArrayBuffer[Int]()
  val stdin = new InputStreamReader(System.in)

  Stream.continually(stdin.read()).takeWhile(_ != -1).foreach(bytes += _)

  Parser.parse(bytes) match {
    case Right(result) => println(result.show)
    case Left(error) => println(error.show)
  }
}
