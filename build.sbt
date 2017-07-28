import Dependencies._

lazy val commonSettings = Seq(
  organization := "coop.rchain",
  scalaVersion := "2.12.2-bin-typelevel-4",
  version := "0.1.0-SNAPSHOT",
  scalaOrganization in ThisBuild := "org.typelevel",
  scalacOptions += "-Ypartial-unification", // enable fix for SI-2712
  scalacOptions += "-Yliteral-types"       // enable SIP-23 implementation
)

lazy val fuzzer = (project in file("fuzzer")).settings(
  commonSettings,
  name := "Fuzzer",
  libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"
)

lazy val connector = (project in file("connector"))
  .settings(
    commonSettings,
    name := "Fuzzer-connector"
  )
  .dependsOn(fuzzer)
