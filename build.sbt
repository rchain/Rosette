import Dependencies._

lazy val commonSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.12.1",
  version := "0.1.0-SNAPSHOT",
  scalafmtOnCompile in ThisBuild := true
)

lazy val fuzzer = (project in file("fuzzer")).settings(
  commonSettings,
  name := "Fuzzer"
)

lazy val connector = (project in file("connector"))
  .settings(
    commonSettings,
    name := "Fuzzer-connector"
  )
  .dependsOn(fuzzer)
