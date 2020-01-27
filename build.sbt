name := "scala-async-examples"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
// ZIO
  "dev.zio" %% "zio"              % "1.0.0-RC17",
  "dev.zio" %% "zio-streams"      % "1.0.0-RC17",
  "dev.zio" %% "zio-interop-cats" % "2.0.0.0-RC10",
  "dev.zio" %% "zio-interop-monix" % "3.1.0.0-RC1",
  "dev.zio" %% "zio-interop-twitter" % "19.12.0.0-RC1",

// Twitter futures
  "com.twitter" %% "util-core" % "19.12.0",

// Cats Effect
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "co.fs2"        %% "fs2-core"    % "2.2.1",
  
// Monix
  "io.monix" %% "monix" % "3.1.0-2156c0e",
)
