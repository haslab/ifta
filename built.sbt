
name := "IFTA"
 
version := "1.0"
 
scalaVersion := "2.12.4"
 
libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    "junit" % "junit" % "4.12",
//    "org.scalatest" % "scalatest_2.11" % "3.0.0-M15",
    //"org.ow2.sat4j" % "org.ow2.sat4j.sat" % "2.3.5"
    "org.scalactic" %% "scalactic" % "3.0.4",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5" withSources() withJavadoc()
    //"org.sat4j" % "org.sat4j.pb" % "2.3.1",
    //"org.sat4j" % "org.sat4j.core" % "2.3.1"
//    "org.jgrapht" % "jgrapht-core" % "1.0.0"
)

excludeFilter in unmanagedSources := "IftaAutomata.scala"