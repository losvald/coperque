name := "CoPerQue"

organization := "org.eold.coperque"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"

scalacOptions ++= Seq("-feature")

parallelExecution in Test := true

// Comment out the '!' to run speed tests instead of correctness tests
testOptions in Test := Seq(Tests.Filter(s => !
  s.endsWith("Speedtest")))

testOptions in Test += Tests.Argument("-oD")
