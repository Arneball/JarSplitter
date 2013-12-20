name := "dexer"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "net.lingala.zip4j" % "zip4j" % "1.3.1",
  "org.apache.httpcomponents" % "httpclient" % "4.3.1"
)     

play.Project.playScalaSettings
