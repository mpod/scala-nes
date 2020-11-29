name := "scala-nes"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.typelevel"              %% "cats-effect"     % "2.0.0",
  "ch.qos.logback"              % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.2",
  "org.scodec"                 %% "scodec-core"     % "1.11.7",
  "org.scodec"                 %% "scodec-stream"   % "2.0.0",
  "co.fs2"                     %% "fs2-core"        % "2.4.0",
  "co.fs2"                     %% "fs2-io"          % "2.4.0",
  "com.github.scopt"           %% "scopt"           % "4.0.0-RC2",
  "org.scalatest"              %% "scalatest"       % "3.2.2" % "test"
)
scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8", "-feature")

mainClass in (Compile, run) := Some("scalanes.Console")

mainClass in assembly := Some("scalanes.Console")
assemblyJarName in assembly := "scala-nes.jar"
