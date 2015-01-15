organization := "org.smop"

name := "euterpea"

version := "0.1.0-SNAPSHOT"

description := "Scala port of Euterpea Haskell music library"

licenses += ("BSD Simplified", url("http://opensource.org/licenses/bsd-license"))

scalaVersion := "2.11.5"

incOptions := incOptions.value.withNameHashing(true)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-effect" % "7.1.0",
  "org.spire-math" %% "spire" % "0.9.0",
  "org.scodec" %% "scodec-bits" % "1.0.5"
)

