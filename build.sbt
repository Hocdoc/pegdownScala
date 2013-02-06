name := "pegdownScala"

version := "0.1"

scalaVersion := "2.10.0"

homepage := Some(new URL("http://pegdown.org"))

organization := "org.pegdown"

organizationHomepage := Some(new URL("http://pegdown.org"))

description := "A Scala library providing a clean and lightweight markdown processor"

startYear := Some(2013)

licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))

libraryDependencies ++= Seq(
  "org.parboiled" % "parboiled-scala_2.10" % "1.1.4",
  "net.sf.jtidy" % "jtidy" % "r938" % "test",
  "org.specs2" %% "specs2" % "1.13" % "test",
  "com.novocode" % "junit-interface" % "0.10-M2" % "test"
)

