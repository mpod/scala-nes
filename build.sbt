name := "scala-nes"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions += "-Ypartial-unification"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.typelevel"              %% "cats-core"       % "2.0.0",
  "org.typelevel"              %% "cats-effect"     % "2.0.0",
  "eu.timepit"                 %% "refined"         % "0.9.10",
  "ch.qos.logback"              % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.2",
  "com.github.julien-truffaut" %% "monocle-core"    % "2.0.0",
  "com.github.julien-truffaut" %% "monocle-macro"   % "2.0.0",
  "com.github.julien-truffaut" %% "monocle-law"     % "2.0.0" % "test",
  "org.scalafx"                %% "scalafx"         % "12.0.2-R18",
  "com.chuusai"                %% "shapeless"       % "2.3.3",
  "org.scodec"                 %% "scodec-core"     % "1.11.4",
  "org.scodec"                 %% "scodec-stream"   % "2.0.0",
  "co.fs2"                     %% "fs2-core"        % "2.4.0",
  "co.fs2"                     %% "fs2-io"          % "2.4.0",
  "org.scalatest"              %% "scalatest"       % "3.0.8" % "test"
)
scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8", "-feature")

// Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
fork := true

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "12.0.2" classifier osName
)