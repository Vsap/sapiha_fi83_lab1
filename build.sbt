name := "sapiha_fi83_lab1"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.0" % "test"
libraryDependencies += "org.specs2" %% "specs2-core" % "4.10.0" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")