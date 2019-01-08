name := "fpmortals"

version := "0.1"

scalaVersion in ThisBuild := "2.12.8"
scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum"     % "0.13.0",
  "org.scalaz"           %% "scalaz-core"    % "7.2.26"
)

libraryDependencies += "com.propensive" %% "contextual" % "1.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "eu.timepit" %% "refined-scalaz" % "0.9.2"
libraryDependencies += "com.fommil" % "jsonformat_2.12" % "1.0.0-RC9"


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

enablePlugins(JmhPlugin)