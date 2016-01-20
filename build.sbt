organization := "org.smop"

name := "euterpea"

version := "0.1.0-SNAPSHOT"

description := "Scala port of Euterpea Haskell music library"

licenses += ("BSD Simplified", url("http://opensource.org/licenses/bsd-license"))

scalaVersion := "2.11.7"

incOptions := incOptions.value.withNameHashing(true)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-effect" % "7.1.6",
  "org.spire-math" %% "spire" % "0.11.0",
  "org.scodec" %% "scodec-bits" % "1.0.12",
  "org.scodec" %% "scodec-core" % "1.8.3"
)

