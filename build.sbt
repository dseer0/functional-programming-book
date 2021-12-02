name := "functionalprogrammingbook"

version := "0.1"

scalaVersion := "2.13.6"


val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= List(
  "org.http4s" %% "http4s-blaze-server" % "0.22.0",
  "org.http4s" %% "http4s-dsl" % "0.22.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)
