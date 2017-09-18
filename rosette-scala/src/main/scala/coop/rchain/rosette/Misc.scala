package coop.rchain.rosette

object Misc {
  def suicide(msg: String): Unit = {
    System.err.println(s"*** fatal error: $msg")
    System.exit(1)
  }
}
